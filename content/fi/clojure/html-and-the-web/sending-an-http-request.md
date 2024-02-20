---
date: 2024-01-20 17:59:28.885700-07:00
description: "HTTP-pyynn\xF6n l\xE4hett\xE4minen on tapa saada tai l\xE4hett\xE4\xE4\
  \ tietoa verkossa toimivien palvelimien v\xE4lill\xE4. Ohjelmoijat k\xE4ytt\xE4\
  v\xE4t sit\xE4 dataa vaihtaakseen, API:hin\u2026"
lastmod: 2024-02-19 22:05:15.122987
model: gpt-4-1106-preview
summary: "HTTP-pyynn\xF6n l\xE4hett\xE4minen on tapa saada tai l\xE4hett\xE4\xE4 tietoa\
  \ verkossa toimivien palvelimien v\xE4lill\xE4. Ohjelmoijat k\xE4ytt\xE4v\xE4t sit\xE4\
  \ dataa vaihtaakseen, API:hin\u2026"
title: "HTTP-pyynn\xF6n l\xE4hett\xE4minen"
---

{{< edit_this_page >}}

## What & Why?
HTTP-pyynnön lähettäminen on tapa saada tai lähettää tietoa verkossa toimivien palvelimien välillä. Ohjelmoijat käyttävät sitä dataa vaihtaakseen, API:hin kytkeytyäkseen tai verkkopalveluja kuluttaakseen.

## How to:
Clojuren HTTP-kirjastot, kuten `clj-http`, tekevät pyyntöjen lähettämisen vaivattomaksi. Esimerkiksi GET-pyynnön tekeminen:

```Clojure
(require '[clj-http.client :as client])

(let [response (client/get "http://httpbin.org/get")]
  (println response))
```

Sample output:
```
{:status 200, :headers { ... }, :body "..."}
```

POST-pyynnön lähettäminen:

```Clojure
(let [response (client/post "http://httpbin.org/post" {:form-params {:key "value"}})]
  (println response))
```

## Deep Dive
Clojure ei tule sisäänrakennetuilla HTTP-ominaisuuksilla, toisin kuin jotkut muut kielet. Sen sijaan, Clojure-koodarit turvautuvat kirjastoihin, kuten `clj-http`. Tämä kirjasto nojaa Java's HttpURLConnectioniin tehden pyyntöjen lähettämisestä helppoa ja joustavaa.

Ennen `clj-http`:ia, yleinen tapa oli käyttää Java-kirjastoja suoraan Clojuresta. Vaikka tämä on yhä vaihtoehto, `clj-http` vapauttaa Clojure-kehittäjät monista alhaan tason yksityiskohdista.

Yksi Clojuristien käyttämä vaihtoehtoinen kirjasto on `http-kit`, joka on kevyempi ja tarkoitettu asynkronisten pyyntöjen käsittelyyn.

## See Also
- `clj-http` dokumentaatio: https://github.com/dakrone/clj-http
- `http-kit` projekti: http://www.http-kit.org/
- Clojuren oman `java.net.http` käyttöesimerkit: https://clojure.org/guides/deps_and_cli
