---
aliases:
- /sv/clojure/sending-an-http-request/
date: 2024-01-20 17:59:14.491633-07:00
description: "Att skicka en HTTP-beg\xE4ran \xE4r hur en dator eller app fr\xE5gar\
  \ efter data fr\xE5n en server eller en tj\xE4nst p\xE5 n\xE4tet. Programmerare\
  \ g\xF6r detta f\xF6r att interagera\u2026"
lastmod: 2024-02-18 23:08:51.459156
model: gpt-4-1106-preview
summary: "Att skicka en HTTP-beg\xE4ran \xE4r hur en dator eller app fr\xE5gar efter\
  \ data fr\xE5n en server eller en tj\xE4nst p\xE5 n\xE4tet. Programmerare g\xF6\
  r detta f\xF6r att interagera\u2026"
title: "Skicka en http-f\xF6rfr\xE5gan"
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
