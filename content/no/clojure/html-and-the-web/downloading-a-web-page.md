---
date: 2024-01-20 17:43:34.459982-07:00
description: "How to: I Clojure kan vi bruke `clj-http` biblioteket for \xE5 laste\
  \ ned nettsider. Her er et kort eksempel."
lastmod: '2024-03-13T22:44:40.401241-06:00'
model: gpt-4-1106-preview
summary: "I Clojure kan vi bruke `clj-http` biblioteket for \xE5 laste ned nettsider."
title: Nedlasting av en nettside
weight: 42
---

## How to:
I Clojure kan vi bruke `clj-http` biblioteket for å laste ned nettsider. Her er et kort eksempel:

```clojure
(require '[clj-http.client :as client])

(defn download-webpage [url]
  (:body (client/get url)))

(println (download-webpage "http://example.com"))
```

Kjøring gir deg HTML-innholdet til `http://example.com`.

## Deep Dive
Nedlasting av nettsider går helt tilbake til webbens barndom. Det handler om å sende en HTTP GET-forespørsel til en server. 

Alternativer til `clj-http` inkluderer `http-kit` og `aleph`. De har sine egne styrker, som non-blocking I/O, som kan være mer effektiv for noen applikasjoner.

Implementasjonen kan involvere mer enn bare å hente innholdet. Tenk på feilhåndtering, tidsavbrudd og håndtering av omdirigeringer. Med `clj-http` kan du konfigurere disse aspektene nøyaktig slik du trenger.

## See Also
- `[clj-http GitHub repo](https://github.com/dakrone/clj-http)`
- `[Official Clojure Documentation](https://clojure.org/guides/getting_started)`
- `[http-kit](http://www.http-kit.org/)`
- `[aleph](https://aleph.io/)`
