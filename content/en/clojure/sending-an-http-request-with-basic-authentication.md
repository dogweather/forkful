---
title:                "Sending an HTTP request with basic authentication"
date:                  2024-01-20T18:01:16.140317-07:00
model:                 gpt-4-1106-preview
simple_title:         "Sending an HTTP request with basic authentication"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## What & Why?

Sending an HTTP request with basic authentication involves adding a username and password to a request for restricted resources. Programmers do it to access APIs or web services that need some level of security.

## How to:

In Clojure, you'll typically use the `clj-http` library for HTTP requests, including those with basic auth. Let's start with adding the dependency (`[clj-http "3.12.3"]` as of my last update) to your `project.clj`.

Next, here's how you craft a GET request with basic authentication:

```clojure
(require '[clj-http.client :as client])

(let [response (client/get "https://your-api.com/resource"
                           {:basic-auth ["username" "password"]})]
  (println "Status:" (:status response))
  (println "Body:" (:body response)))
```
Replace `"https://your-api.com/resource"`, `"username"`, and `"password"` with your details. This code sends a GET request and prints the status and body of the response.

Sample output could look something like this:

```
Status: 200
Body: {JSON data or something else here}
```

## Deep Dive

HTTP Basic Authentication has roots in early web protocols. It passes the username and password in an HTTP header encoded using Base64. While it's simple, it's not the most secure since the credentials can be easily decoded if intercepted.

Alternatives:
- **Digest Authentication**: More complex, involves sending a hashed version of the password instead.
- **OAuth**: A more robust system for authorization that doesn't require sending username and password.
- **API Keys**: Unique tokens used instead of traditional login credentials.

Under the hood in `clj-http`, specifying `:basic-auth` in the options hashmap triggers the library to encode your credentials and tack them onto the HTTP `Authorization` header. When the server gets the request, it decodes the header and checks the credentials.

Keep in mind that for secure transmission, HTTPS should be used to prevent others from intercepting your credentials.

## See Also

- clj-http GitHub repo: https://github.com/dakrone/clj-http
- Clojure Official Documentation: https://clojure.org/
- HTTP Authentication on MDN: https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication 
- Understanding OAuth: https://oauth.net/
