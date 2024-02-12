---
title:                "Skicka en HTTP-förfrågan med Basic-autentisering"
aliases:
- /sv/clojure/sending-an-http-request-with-basic-authentication.md
date:                  2024-01-20T18:01:29.900699-07:00
model:                 gpt-4-1106-preview
simple_title:         "Skicka en HTTP-förfrågan med Basic-autentisering"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att skicka en HTTP-förfrågan med grundläggande autentisering innebär att man inkluderar användarnamn och lösenord för tillgångskontroll. Programmerare gör detta för att säkerställa att endast behöriga användare kan utföra vissa åtgärder på en server.

## Hur man gör:
```Clojure
(require '[clj-http.client :as client])

(defn send-authenticated-request [url username password]
  (let [credentials (str username ":" password)
        encoded-creds (-> credentials (.getBytes) java.util.Base64/getEncoder (.encodeToString))]
    (client/get url {:headers {"Authorization" (str "Basic " encoded-creds)}})))

;; Använd din URL och credentials här
(def response (send-authenticated-request "http://example.com" "minAnvändare" "mittLösenord"))

(println response)
```

Output exempel:
```Clojure
{:status 200, :headers {...}, :body "...", ...}
```

## Fördjupning
För länge sedan, innan HTTPS blev standard, var grundläggande autentisering över HTTP mer vanligt. Det är en enkel mekanism där användarnamn och lösenord kombineras och kodas om till Base64. Alternativ till detta inkluderar OAuth, API-nycklar och moderna autentiseringsscheman som JWT (JSON Web Tokens).

I Clojure sköter vi HTTP-förfrågningar med hjälp av tredjepartsbibliotek som `clj-http`. Grundläggande autentisering via `clj-http` kräver encoding av användarnamnet och lösenordet till Base64 och sedan läggs de till i `Authorization`-headern av förfrågan. Vi bör vara medvetna om att utan en säker HTTP-anslutning (HTTPS), kan känslig information potentiellt avlyssnas.

Trots dess brister i säkerhet, kan grundläggande autentisering fortfarande vara användbart vid tester eller inom interna nätverk där informationen inte är känslig eller redan säkrad på annat sätt.

## Se även
- [HTTP authentication on MDN](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
- [clj-http GitHub repository](https://github.com/dakrone/clj-http)
