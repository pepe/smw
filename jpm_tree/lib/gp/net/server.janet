(defmacro supervisor
  ```
  Simple supervisor with handling new connection. 
  And closing the connection.
  ```
  [chan handling & rules]
  (assert (even? (length rules)) "Rules must be pairs")
  (def default-rules
    ~[,;rules
      [:close connection] (:close connection)
      [:conn connection]
      (ev/go
        (fiber/new
          (fn handling-connection [conn]
            (setdyn :conn conn)
            (,handling conn)) :tp) connection ,chan)])
  ~(forever (match (ev/take ,chan) ,;default-rules)))

(defn start
  ```
  This function starts the server. Usually in the fiber.
  
  It takes channel to which it will put incomming connection under tag `:conn`.

  It takes two optional arguments:
  - `host` on which server starts. Default `localhost`
  - `port` on which server starts. Default `8888`
  ```
  [chan &opt host port]
  (default host "localhost")
  (default port "8888")
  (ev/go
    (fiber/new
      (fn accept-connection [server]
        (forever (ev/give-supervisor :conn (net/accept server)))))
    (net/listen host port) chan))

(defmacro spawn
  ```
  Spawns new server with handling, host port and rules.
  
  It takes two required parameters:
  - `supervisor` supervisor macro you want to use, usually one of specialized for http, ws
    or rpc.
  - `handling` handling function, usually composed by specific `on-connection` function
    and handler function or object.
  
  It also takes three optional parameters:
  - `host` hostname to bind to.
  - `port` port to bind to.
  - `rules` variadic rules' pairs for the supervisor pattern matching.
  
  It returs the supervisor channel.
  ```
  [supervisor handling &opt host port & rules]
  (with-syms [chan h]
    ~(let [,chan (ev/chan)]
       (ev/spawn
         (,start ,chan ,host ,port)
         (as-macro ,supervisor ,chan ,handling ,;rules))
       ,chan)))

(defn host-port
  "Splits connection string into host and port parts."
  [conns]
  (string/split ":" conns))
