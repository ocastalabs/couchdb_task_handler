CouchDB Task Handler
====================

This is a rudimentary alternative to writing _changes listeners. In deployments where there are a large number of databases (for instance, one per user) _changes seemed like an expensive overhead particularly when most databases were inactive so we came up with an alternative approach.

Whenever a PUT, POST or DELETE occurs in a datadase whose name matches a configured regex expression task_handler invokes some python code. In the case of the example this simply creates a Celery task passing in the database name and document ID and rev.

In order for this handler to work the changes defined in the Ocasta Labs CouchDB Clone (branch [ id\_and\_rev\_in\_response\_headers)](https://github.com/ocastalabs/couchdb/tree/id_and_rev_in_response_headers)) are required.

Building the handler
--------------------

    rebar compile

Installing the handler
----------------------

Simply link the directory to the the place all your other couchdb Erlang libraries are. e.g.

    sudo ln -s /home/myuser/couchdb_task_handler/ /usr/local/lib/couchdb/erlang/lib/task_handler

Configuration
-------------

Add

    [daemons]
    task_handler_python={task_handler_sup, start_link, []}

    [task_handler]
    db_regex=^(?!_).*
    
to local.ini. This will trigger the python code for any database where the name doesn't start with an underscore