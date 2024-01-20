---
title:                "Sending an http request with basic authentication"
html_title:           "Fish Shell recipe: Sending an http request with basic authentication"
simple_title:         "Sending an http request with basic authentication"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## What & Why?
Sending an HTTP request with basic authentication in Clojure means crafting an HTTP call that includes user credentials. It's usually done to secure access to web resources, ensuring that only authenticated users can interact with a given endpoint. 

## How to:

Clojure's clj-http library simplifies the job. To send a GET request with Basic Authentication, you'll need to:

```Clojure
(require '[clj-http.client :as client])

(defn fetch-resource
  [url username password]
  (client/get url {:basic-auth [username password]}))
```

Calling `fetch-resource` with URL, username, and password will return the response from the server:

```Clojure
(fetch-resource "http://example.com" "user" "pass")
```

## Deep Dive

Basic Authentication has been a part of HTTP since the early days. Its simplicity made it popular: you send the username and password, base64 encoded, in the `Authorization` header of each request. However, encoding is not encryption, so it's not secure without use of HTTPS.

Clojure offers tackle this through libraries like `clj-http` for providing options like basic authentication. Other options exist as well: OAuth and JWT implementations can also be found in the ecosystem, should your application need more advanced authentication features. 

Under the hood, `clj-http` is merely setting the `Authorization` header for you:

```Clojure
{:headers {"Authorization" (str "Basic " (base64 (str username ":" password)))}}
```

## See Also

1. [clj-http library](https://github.com/dakrone/clj-http): Explore the Clojure library simplifying HTTP requests.
2. [Clojure.org](https://clojure.org/): Resources, guides, and community for those engaged in Clojure.
3. [HTTP Basic Access Authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication): Deep-dive into the world of HTTP authentication.