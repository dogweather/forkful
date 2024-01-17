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

## What & Why?

Sending an HTTP request with basic authentication means including a username and password in the request in order to authenticate and gain access to a protected resource. Programmers use this technique to restrict access to sensitive information and ensure that only authorized users can access it.

## How to:

```Clojure
(require '[clj-http.client :as client])

(def url "https://example.com/api/v1/users") ; URL of the protected resource
(def username "user123") ; replace with actual username
(def password "pass456") ; replace with actual password

; Sending a GET request with basic authentication
(client/get url :basic-auth [username password]) ; returns the response from the API

; Sending a POST request with basic authentication
(client/post url :body {"name" "John Doe"} :basic-auth [username password]) ; returns the response from the API
```

## Deep Dive

Sending an HTTP request with basic authentication dates back to the early days of the web when security was not a major concern. It involves sending the username and password encoded in base64 format in the 'Authorization' header of the request. This method is considered less secure as the credentials are transmitted in plaintext, making it vulnerable to malicious attacks.

An alternative to basic authentication is using OAuth, which uses a token-based system for authorization. This method is more secure as it does not require exchanging and storing sensitive credentials.

Implementation details for sending an HTTP request with basic authentication differ depending on the programming language and framework. Clojure has a built-in library - clj-http - specifically designed for making HTTP requests. It provides a simple and easy-to-use API for sending requests with basic authentication.

## See Also:

- [clj-http library docs](https://www.http-kit.org/client.html)
- [more info on basic authentication](https://www.ietf.org/rfc/rfc2617.txt)
- [OAuth overview](https://oauth.net/2/)