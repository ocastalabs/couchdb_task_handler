-module(task_handler_db).
-include_lib("couch/include/couch_db.hrl").
-export([handle_request/1]).

handle_request(#httpd{
                  path_parts=[DbName|RestParts],
                  method=Method
                 }=Request)->
    {ok, Response} = couch_httpd_db:handle_request(Request),
    %% Master-slave replication
    if 
        Method =:= 'PUT' orelse Method =:= 'POST' orelse Method =:= 'DELETE' ->
            MasterURL = couch_config:get("master_slave_replication", "master_url"),
            SlaveURL = couch_config:get("master_slave_replication", "slave_url"),
            trigger_slave_replication(MasterURL, SlaveURL, Method, binary_to_list(DbName), RestParts);
        true ->
            ok
    end,
    %% Task creation
    ResponseHeaders = Response:get(headers),
    IsRewrite = lists:member(<<"_rewrite">>, RestParts),
    if
        (Method =:= 'PUT' orelse Method =:= 'POST') and not IsRewrite ->
            {Id, ETag, UpdateRev} = {
              mochiweb_headers:get_value("x-couch-id", ResponseHeaders),
              mochiweb_headers:get_value("etag", ResponseHeaders),
              mochiweb_headers:get_value("x-couch-update-newrev", ResponseHeaders)},
            Enabled = couch_config:get("task_handler", "enabled"),
            trigger_task_creation(Enabled, Id, ETag, UpdateRev, DbName);
        true ->
            ok
    end,
    {ok, Response}.

trigger_task_creation("true", Id, ETag, UpdateRev, DbName) ->
    Rev = case ETag of
              undefined -> 
                  UpdateRev;
              _ ->
                  ETagLength = length(ETag),
                  ETagWithoutQuotes = string:substr(ETag, 2, ETagLength - 2),
                  ETagWithoutQuotes
          end,
    try 
        DBRegEx = couch_config:get("task_handler", "db_regex", "dbz_.*"),
        case re:run(binary_to_list(DbName), DBRegEx) of
            {match, _} ->
                ok = task_handler_python_server:create_task(binary_to_list(DbName), Id, Rev);
            nomatch ->
                ok
        end
    catch 
        _:_ ->
            ok
    end;
trigger_task_creation(_, _, _, _, _) ->
    ok.


trigger_slave_replication(undefined, _, _, _, _) ->
    ok;
trigger_slave_replication(_, undefined, _, _, _) ->
    ok;
trigger_slave_replication(_MasterURL, SlaveURL, 'DELETE', DbName, []) ->
    %% Delete the DB from the slave
    handle_response(httpc:request(delete, {SlaveURL ++ "/" ++ DbName, []}, [], []), DbName);
trigger_slave_replication(MasterURL, SlaveURL, _, DbName, _) when is_list(DbName) ->
    %% Replicate docs
    Replicate = 
        fun() ->
                PostContent = iolist_to_binary(io_lib:format("{\"source\":\"~s/~s\",\"target\":\"~s\", \"create_target\":true}", [MasterURL, DbName, DbName])),
                Response = httpc:request(post, {SlaveURL ++ "/_replicate", [], "application/json", PostContent}, [], []),
                handle_response(Response, DbName)
        end,
    spawn(Replicate).

handle_response({ok, {{_, Code, _}, _Headers, Body}}, DbName) ->
    case Code of
        200 ->
            ?LOG_DEBUG("~s replicated to slave", [DbName]);
        _ ->
            ?LOG_ERROR("could not replicate ~s to slave: [~p] ~p", [DbName, Code, Body])
    end;
handle_response({error, Reason}, DbName) ->
    ?LOG_ERROR("could not replicate ~s to slave: ~p", [DbName, Reason]).



