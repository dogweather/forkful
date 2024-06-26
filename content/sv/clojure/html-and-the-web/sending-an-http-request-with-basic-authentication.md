---
date: 2024-01-20 18:01:29.900699-07:00
description: "Hur man g\xF6r: F\xF6r l\xE4nge sedan, innan HTTPS blev standard, var\
  \ grundl\xE4ggande autentisering \xF6ver HTTP mer vanligt. Det \xE4r en enkel mekanism\
  \ d\xE4r anv\xE4ndarnamn\u2026"
lastmod: '2024-04-05T22:50:51.808612-06:00'
model: gpt-4-1106-preview
summary: "F\xF6r l\xE4nge sedan, innan HTTPS blev standard, var grundl\xE4ggande autentisering\
  \ \xF6ver HTTP mer vanligt."
title: "Skicka en HTTP-f\xF6rfr\xE5gan med Basic-autentisering"
weight: 45
---

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
