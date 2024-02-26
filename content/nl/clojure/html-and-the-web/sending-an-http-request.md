---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:07:29.441982-07:00
description: "Een HTTP-verzoek verzenden is hoe je programma een ander systeem vraagt\
  \ om gegevens of diensten over het web. Programmeurs doen dit om te interageren\
  \ met\u2026"
lastmod: '2024-02-25T18:49:47.809876-07:00'
model: gpt-4-0125-preview
summary: "Een HTTP-verzoek verzenden is hoe je programma een ander systeem vraagt\
  \ om gegevens of diensten over het web. Programmeurs doen dit om te interageren\
  \ met\u2026"
title: Een HTTP-verzoek verzenden
---

{{< edit_this_page >}}

## Wat & Waarom?
Een HTTP-verzoek verzenden is hoe je programma een ander systeem vraagt om gegevens of diensten over het web. Programmeurs doen dit om te interageren met web-API's, bronnen op te halen of communicatie tussen diensten mogelijk te maken.

## Hoe?
In Clojure kun je HTTP-verzoeken verzenden met de `clj-http` client.

Voeg eerst de afhankelijkheid toe aan je `project.clj`:
```clojure
[clj-http "3.12.3"]
```

Laten we nu een GET-verzoek verzenden:
```clojure
(require '[clj-http.client :as client])

(let [reactie (client/get "http://httpbin.org/get")]
  (println reactie))
```

Voorbeeld van uitvoer:
```clojure
{:status 200, :headers {...}, :body "..."}
```

Om gegevens te posten:
```clojure
(let [reactie (client/post "http://httpbin.org/post" {:form-params {:key "value"}})]
  (println reactie))
```

## Diepere Duik
HTTP-verzoeken verzenden is niet nieuw. Het is zo oud als het web zelf. Clojure, zijnde een moderne Lisp, heeft verschillende bibliotheken om HTTP-verzoeken te maken. `clj-http` is een populaire, maar er bestaan ook anderen zoals `http-kit` of het kernonderdeel van Clojure `clj-http.client`.

`clj-http` leunt op de Apache HttpComponents Client voor Java onder de motorkap. Het is veelzijdig maar kan aanvoelen als zwaar Java-georiënteerd. Een alternatief, `http-kit`, is lichter en meer Clojure-idiomatisch maar minder rijk aan functies.

Wanneer je HTTP-verzoeken verzendt, doe je dit via TCP/IP, wat je verzoeken inkadert volgens een goed gevestigd protocol. Deze universele standaard stelt je in staat om met praktisch elke webdienst daarbuiten te interageren.

## Zie Ook
- `clj-http` GitHub-repository: https://github.com/dakrone/clj-http
- Officiële Clojure-site: https://clojure.org
- Documentatie van HttpComponents Client: https://hc.apache.org/httpcomponents-client-ga/
- Voor real-time behoeften, overweeg `http-kit`: http://www.http-kit.org
