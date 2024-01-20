---
title:                "Skicka en http-begäran med grundläggande autentisering"
html_title:           "Elixir: Skicka en http-begäran med grundläggande autentisering"
simple_title:         "Skicka en http-begäran med grundläggande autentisering"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att skicka en HTTP-förfrågan med grundläggande autentisering innebär att vi skickar data över internet genom HTTP (Hyper Text Transfer Protocol), samtidigt som vi verifierar användaren genom en autentiseringsprocess. Detta gör vi för att hålla data säkert och förhindra obehörig åtkomst.

## Hur man gör:
Vi använder `clj-http.client` biblioteket för att skicka en HTTP-förfrågan med grundläggande autentisering. Så här gör vi det:

1. Installera `clj-http.client` biblioteket. Lägg till följande i ditt `project.clj` fil:

```Clojure
[clj-http "3.12.2"]
```
   
2. Skicka HTTP-förfrågan med grundläggande autentisering:

```Clojure
(ns my-app.core
  (:require [clj-http.client :as client]))

(defn get-request []
  (client/get "http://example.com" {:basic-auth ["username" "password"]}))
```

Detta skulle ge en output som ser ut som följande:

```Clojure
{:status 200
 :headers {"content-type" "application/json; charset=UTF-8"}
 :body "{\"message\":\"Hello, world!\"}"
 :trace-redirects ["http://example.com"]}
```
## Djupdykning

Att skicka en HTTP-förfrågan med grundläggande autentisering har varit en standard sedan HTTP/1.0. 

Alternativ till grundläggande autentisering inkluderar OAuth och OpenID. Men grundläggande autentisering är ofta snabbare och enklare att implementera, särskilt för enklare tillämpningar.

När det gäller genomförandet skickar `clj-http.client` biblioteket en GET-förfrågan till den angivna URL:en med autentiseringsparametrar i HTTP-huvudet. Den kodade strängen (som är bas64) av användarnamn och lösenord används för autentisering.

## Se även

Här är några resurser där du kan lära dig mer om att skicka HTTP-förfrågningar med grundläggande autentisering:

- [HTTP authentication: Basic and Digest Access Authentication.](https://tools.ietf.org/html/rfc2617)
- [clj-http Github documentation.](https://github.com/dakrone/clj-http)
- [Clojure HTTP library tutorial.](https://www.baeldung.com/clojure-http-library)