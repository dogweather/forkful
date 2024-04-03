---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:59:08.468900-07:00
description: "Een webpagina downloaden betekent het HTML van een URL ophalen, zodat\
  \ je programma ermee kan werken. Programmeurs doen dit om gegevens te schrapen,\u2026"
lastmod: '2024-03-13T22:44:50.418080-06:00'
model: gpt-4-0125-preview
summary: Een webpagina downloaden betekent het HTML van een URL ophalen, zodat je
  programma ermee kan werken.
title: Een webpagina downloaden
weight: 42
---

## Hoe te:
In Clojure kun je `clj-http` gebruiken om snel een webpagina te downloaden. Hier is een eenvoudig voorbeeld:

```Clojure
(require '[clj-http.client :as client])

(defn download-page [url]
  (client/get url))

;; Gebruik het als volgt:
(defn -main []
  (println (download-page "http://example.com")))
```

Als je dat uitprobeert, krijg je een map vol details. De interessante delen bevinden zich onder `:body` en `:status`.

## Diepere duik
Historisch gezien was het downloaden van webpagina's een "wget" of "curl" op de commandoregel. Nu abstraheren talen zoals Clojure dit met bibliotheken. `clj-http` is zo'n bibliotheek die Java's Apache HttpComponents inpakt voor Clojures functionele stijl.

Alternatieven? Zeker. Je zou direct `java.net.HttpURLConnection` kunnen gebruiken of een andere bibliotheek zoals `http-kit` kunnen kiezen - maar `clj-http` is comfortabel en bevat de meeste dingen die je direct uit de doos nodig hebt.

Wat betreft de technische details, `clj-http` zet je verzoek om in een Java HTTP-entiteit, maakt de oproep en geeft de reactie terug. Achter de schermen houdt het zich bezig met omleidingen, het parsen van koppen en het beheren van het antwoordlichaam, zodat je je kunt concentreren op je gegevens, niet op het leidingwerk.

## Zie ook
- clj-http GitHub-repo: [https://github.com/dakrone/clj-http](https://github.com/dakrone/clj-http)
- Clojure http-kit voor een andere aanpak: [http://www.http-kit.org](http://www.http-kit.org)
- Officiële Clojure-site voor meer over de taal: [https://clojure.org](https://clojure.org)
