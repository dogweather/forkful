---
title:                "Skicka ett http-förfrågan med grundläggande autentisering"
html_title:           "Clojure: Skicka ett http-förfrågan med grundläggande autentisering"
simple_title:         "Skicka ett http-förfrågan med grundläggande autentisering"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Vad & Varför? 
Skicka en HTTP-begäran med grundläggande autentisering är en metod för att autentisera begäran när du kommunicerar med en server via HTTP-protokollet. Programutvecklare använder denna metod för att säkerställa att endast auktoriserade användare har åtkomst till resurser som finns på servern.

## Hur man gör:
Här är ett exempel på hur du kan skicka en HTTP-begäran med grundläggande autentisering i Clojure:
```Clojure
(require '[clj-http.client :as client])
(def response (client/get "https://example.com/api/resource"
                          :basic-auth "username" "password"))
```

Det första steget är att importera ```clj-http.client``` biblioteket. Sedan definierar vi variabeln ```response``` och tilldelar den resultatet av vår begäran med ```clj-http.client/get``` funktionen. Där vi anger den URL som vi vill skicka begäran till och ```basic-auth``` för att ange användarnamn och lösenord.

## Djupdykning:
Grundläggande autentisering för HTTP-begäran infördes ursprungligen i HTTP 1.0-standarden för att möjliggöra enkel autentisering mellan klient och server. Det finns också andra metoder för autentisering, som token-baserad autentisering, som erbjuder bättre säkerhet. Implementeringen av HTTP-begäran med grundläggande autentisering skiljer sig också åt mellan olika programmeringsspråk.

## Se även:
För mer information om grundläggande autentisering och hur det används i Clojure, ta en titt på Clojures officiella dokumentation för ```clj-http.client``` biblioteket: https://github.com/dakrone/clj-http. Du kan också läsa mer om HTTP-autentisering generellt på Mozillas utvecklarportal: https://developer.mozilla.org/sv/docs/Web/HTTP/Authentication.