---
title:                "Skicka en http-förfrågan"
aliases:
- /sv/clojure/sending-an-http-request/
date:                  2024-01-20T17:59:14.491633-07:00
model:                 gpt-4-1106-preview
simple_title:         "Skicka en http-förfrågan"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att skicka en HTTP-begäran är hur en dator eller app frågar efter data från en server eller en tjänst på nätet. Programmerare gör detta för att interagera med webbaserade API:er, hämta webbsidor, eller kommunicera mellan tjänster.

## Hur gör man:
För att skicka en HTTP-förfrågan i Clojure, kan du använda `clj-http` biblioteket. Installera det först med Leiningen eller Boot i ditt projekt. Här är ett enkelt exempel:

```Clojure
(require '[clj-http.client :as client])

(def response (client/get "https://api.example.com/data"))

(println (:status response))
(println (:headers response))
(println (:body response))
```

Exempel output för ovanstående kod:
```
200
{"Content-Type" "application/json; charset=UTF-8", ...}
{"name":"Example","type":"ExampleType"}
```

## Fördjupning:
Förr använde Clojure-bibliotek som `clj-http-lite` eller `http-kit` för enklare och mer asynkrona förfrågningar. `clj-http` använder Apache HttpComponents och stöder synkrona och asynkrona anrop. Det ger även detaljerad konfiguration, som att hantera cookies och komplexa autentiseringsflöden. 

HTTP-begärningar spelar en stor roll i moderna system med tjänster som kommunicerar via RESTful- eller GraphQL API:er. I Clojure-världen håller `clj-http` kvar sin popularitet för sin enkelhet och kraftfull flexibilitet.

## Se även:
- `clj-http` dokumentation: https://github.com/dakrone/clj-http
- Clojure officiell sida: https://clojure.org/
- Web API-utveckling med Clojure: https://www.clojureforthebraveandtrue.com/
