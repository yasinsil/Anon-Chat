;;(define-keyset 'board-keyset (read-keyset "board-keyset"))
;;(namespace "free")

; Module name
(module board-chat GOVERNANCE

  ; Only admin can upgrade contract
  (defcap GOVERNANCE ()
    ; Enforce keyset
    (enforce-guard (keyset-ref-guard 'board-keyset)))

  ; Constant defining time required without messages to be archived
  (defconst threadlife 86400
    " Time in seconds before threads get archived. ")

  ; Schema & Table containing every messages
  (defschema board-schema thread:string time:time username:string message:string)
  (deftable board-table:{board-schema})

  ; Schema & Table containing threads
  (defschema thread-schema name:string last-message:time)
  (deftable thread-table:{thread-schema})

  ; Schema & Table containing the last message id
  (defschema last-id-schema last-id:integer)
  (deftable last-id:{last-id-schema})

  ; Function to enforce time
  (defun enforce-time:bool (date:time msg:string test:bool)
  @doc " Enforce time. "
    ; Either work or return this error
    (enforce test (format "Chain time must be {} {}" [msg (add-time date threadlife)])))

  ; Function to enforce time before T
  (defun enforce-at-before-date:bool (time:time)
  @doc " Archive threads after {threadlife} without new messages. "
    ; Enforce thread is alive
    (enforce-time time "before"(< (at "block-time" (chain-data)) (add-time time threadlife))))

  ; Function to create a new thread
  (defun newthread (name:string thread:string)
  @doc " Open a new thread. "
     ; Enforce thread length
     (enforce (>= (length thread) 8) "thread name is too short")
     (enforce (<= (length thread) 512) "thread name is too long")
     ; Only allow base64 or fail
     (base64-decode thread)
       ; Insert the thread in the database
       (insert thread-table name {"name": name, "last-message" : (at "block-time" (chain-data))})
       ; Feedback it completed
       (format "{} created" [thread]))

  ; Function to send a new message
  (defun newmessage (thread:string username:string message:string)
  @doc " Send a new message. "
     ; Enforce message length
     (enforce (>= (length message) 8) "Message is too short")
     (enforce (<= (length message) 1024) "Message is too long")
     ; Only allow base64 or fail
     (base64-decode thread) (base64-decode username) (base64-decode message)
     ; Read the thread table and look at the last message
     (with-default-read thread-table thread {"last-message": (time "0000-00-00T00:00:00Z")} { "last-message" := last}
       ; Enforce the thread is not archived
       (enforce-at-before-date last)
       ; Update thread to reset archival countdown
       (update thread-table thread {"last-message": (at "block-time" (chain-data))}))
     ; Read last message id
     (with-default-read last-id "" {"last-id": 0} { "last-id" := last }
       ; Insert the message in the database
       (write board-table (format "{}" [(+ last 1)]) {"thread": thread, "time": (at "block-time" (chain-data)), "username": username, "message": message })
       ; Increment last message id
       (write last-id "" {"last-id": (+ last 1)}))
     ; Feedback it completed
     (format "{} Said {} on {}" [username,message,thread]))

  ; Function to get active threads
  (defun getactivethreads ()
  @doc " Query active threads. "
    ; Read thread table and select those with last messages no older than {threadlife}
    (select thread-table [ 'last-message,'name ] (where 'last-message (< (add-time (at "block-time" (chain-data)) (* threadlife -1))))))

  ; Function to get all threads, even archived ones
  (defun getallthreads ()
  @doc " Query all threads. "
    ; Map everything
    (map (read thread-table) (keys thread-table)))

  ; Function to get all messages from a thread
  (defun getmessages (thread:string)
  @doc " Query messages from a thread. "
    ; Read board table and select messages containing the right thread
    (select board-table [ 'thread,'username,'message,'time ] (where 'thread (= thread))))

  ; Function to get all messages
  (defun getallmessages ()
  @doc " Query all messages. "
    ; Map everything
    (map (read board-table) (keys board-table)))

)

; Create tables for the data
(create-table board-table)
(create-table thread-table)
(create-table last-id)
; Set message id to 0
(write last-id "" {"last-id": 0})
