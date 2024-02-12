---
title:                "Een HTTP-verzoek verzenden"
aliases: - /nl/clojure/sending-an-http-request.md
date:                  2024-01-28T22:07:29.441982-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een HTTP-verzoek verzenden"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/clojure/sending-an-http-request.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
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
