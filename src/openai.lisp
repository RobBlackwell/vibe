;;;; A simple interface to the OpenAI API.  See
;;;; https://platform.openai.com/docs/api-reference

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :drakma)
  (ql:quickload :flexi-streams)
  (ql:quickload :jsown))

(defpackage :openai
  (:use :cl)
  (:export
   :*openi-api-key*
   :list-models
   :completions))

(in-package :openai)

(defparameter *openai-api* "https://api.openai.com/v1/")

(defparameter *openai-api-key* nil)

(defun get-openai-api-key ()
  "Return the OPENAI API key."
  (unless *openai-api-key*
    (setf *openai-api-key* (uiop:getenv "OPENAI_API_KEY")))
  *openai-api-key*)

(defun default-completions-handler (response)
  "Return the completion text from an API completions response."
  (let* ((alist (rest response))
         (choices (rest (second (assoc "choices" alist :test #'string=))))
         (message (cddr (assoc "message" choices :test #'string=)))
         (content (cdr (assoc "content" message :test #'string=))))
    content))

(defun completions (messages &key
                             (handler #'default-completions-handler)
                             (endpoint (concatenate 'string *openai-api* "chat/completions"))
                             (max-tokens nil)
                             (model "gpt-5")
                             (n nil)
                             (temperature nil)
                             (top-p nil)
                             (frequency-penalty nil)
                             (presence-penalty nil)
                             (api-key (get-openai-api-key)))
  "Call the OpenAI completions API"
  (let* ((additional-headers (list  (cons "Authorization" (concatenate 'string "Bearer " api-key))))
         (messages (if (stringp messages)
                       (make-array 1 :initial-contents (list
                                                        (list :obj
                                                              (cons "role" "user")
                                                              (cons "content" messages))))
                       messages))
         (data `(:obj
            ,(cons "messages" messages)
            ,@(when temperature (list (cons "temperature" temperature)))
            ,@(when max-tokens (list (cons "max_tokens" max-tokens)))
            ,@(when top-p (list (cons "top_p"  top-p)))
            ,@(when frequency-penalty (list (cons "frequency_penalty" frequency-penalty)))
            ,@(when presence-penalty (list (cons "presence_penalty" presence-penalty)))
            ,@(when n (list (cons "n" n)))
            ("model" . ,model)))
         (json (jsown:to-json data))
         (response (drakma:http-request endpoint
                                        :method :post
                                        :content-type "application/json"
                                        :additional-headers additional-headers
                                        :content json))
         )
    (funcall handler (jsown:parse (flexi-streams:octets-to-string response)))))


(defun filter-model-names (response)
  "Return the list of model names from a list-models response."
  (let* ((alist (rest response))
         (data (cddr (assoc "data" alist :test #'string=))))
    (mapcar #'(lambda (x)  (cdr (assoc "id" (cdr x) :test #'string=))) data)))

(defun list-models (&key
                      (handler #'filter-model-names)
                      (endpoint (concatenate 'string *openai-api* "engines"))
                      (api-key (get-openai-api-key)))
  "List the available OpenAI models."
  (let* ((additional-headers (list  (cons "Authorization" (concatenate 'string "Bearer " api-key))))
         (response (drakma:http-request endpoint
                                        :method :get
                                        :content-type "application/json"
                                        :additional-headers additional-headers)))


    (funcall handler (jsown:parse (flexi-streams:octets-to-string response)))))
