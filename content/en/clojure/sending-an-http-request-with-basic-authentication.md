---
title:                "Sending an http request with basic authentication"
html_title:           "Clojure recipe: Sending an http request with basic authentication"
simple_title:         "Sending an http request with basic authentication"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Why

Sending an HTTP request with basic authentication is a common and essential task in web development. It allows you to securely access and authenticate with APIs or websites that require login credentials.

## How To

```Clojure
;; First, import the necessary libraries
(require '[clj-http.client :as client])
(require '[clojure.edn :as edn])

;; Define the request URL
(def url "https://example.com/api")

;; Set the basic authentication credentials
(def username "myusername")
(def password "mypassword")

;; Specify the authentication type as "basic"
(def auth {:basic-auth [username password]})

;; Make the HTTP request with basic authentication
(client/post url
             {:auth auth})

;; You can also send additional request parameters
(client/post url
             {:auth auth
              :params (edn/read-string "{\"param1\": \"value1\", \"param2\": \"value2\"}")})
```

Expected output:

```
{:status 200, :headers {"content-type" "application/json"}, :body "{\"message\": \"Success!\"}"}
```

## Deep Dive

Sending an HTTP request with basic authentication involves adding an "Authorization" header to the request. This header contains the word "Basic" followed by a base64-encoded string of the format "username:password". For example, if the username is "myusername" and the password is "mypassword", the base64-encoded string would be "bXl1c2VybmFtZTpteXBhc3N3b3Jk".

Additionally, some APIs or websites may require the use of HTTPS instead of HTTP for secure communication. In this case, you would simply change the request URL to "https://example.com/api".

## See Also

- Clj-http Library: https://github.com/dakrone/clj-http
- Basic Authentication Wikipedia Page: https://en.wikipedia.org/wiki/Basic_access_authentication