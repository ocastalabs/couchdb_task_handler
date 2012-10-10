from erlport import Port, Protocol, String

import sys
sys.path.append('/usr/local/etc/mycelerytasks')
from private_db_celery import doc_changed

class TaskCreator(Protocol):
    """ See http://erlport.org/ for an example """
    def handle_create(self, db_name, id, rev):
        doc_changed.delay(String(db_name), String(id), String(rev))

if __name__ == "__main__":
    proto = TaskCreator()
    proto.run(Port(use_stdio=True))