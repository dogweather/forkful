---
title:                "Å tolke html"
html_title:           "Clojure: Å tolke html"
simple_title:         "Å tolke html"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/parsing-html.md"
---

{{< edit_this_page >}}

## Hvorfor

Parsing av HTML, eller å konvertere HTML-kode til strukturerte data, er en viktig del av webutvikling og dataanalyse. Det lar deg trekke ut informasjon fra nettsider og gjøre den tilgjengelig for behandling i andre programmeringsspråk.

## Hvordan

For å kunne parsere HTML i Clojure, kan du bruke biblioteket "enlive". Først må du importere biblioteket og instruere prosjektet ditt til å bruke det:

```Clojure
(ns minprosjekt.core
 (:require [net.cgrand.enlive-html :refer [html-snippet]])
 (:import (javax.swing.text.html HTMLEditorKit)))
```

Deretter kan du bruke følgende funksjoner for å hente data fra en nettside:

```Clojure
(def html (html-snippet "<p>Hei, verden!</p>"))

(enlive/html-select html [:p]) ; => [<p>Hei, verden!</p>]
(enlive/html-text html) ; => "Hei, verden!"
```

Du kan også navigere gjennom flere elementer:

```Clojure
(def html (html-snippet "<ul><li>Et</li><li>To</li><li>Tre</li></ul>"))

(enlive/html-select html [:ul :li]) ; => [<li>Et</li> <li>To</li> <li>Tre</li>]
```

For å hente ut spesifikke attributter, kan du bruke funksjonen `enlive/html-attr`:

```Clojure
(def html (html-snippet "<a href="https://www.example.com">En lenke</a>"))

(enlive/html-attr html :href) ; => https://www.example.com
```

## Dypdykk

Enlive-biblioteket lar deg også manipulere og bygge HTML-dokumenter. Du kan for eksempel fjerne eller endre eksisterende elementer, eller legge til nye elementer i dokumentet.

For å fjerne et element, kan du bruke funksjonen `enlive/remove`:

```Clojure
(def html (html-snippet "<p>Hei, verden!</p>"))

(enlive/remove html [:p]) ; => <p></p>
```

For å endre et element, kan du bruke funksjonen `enlive/defsnippet`:

```Clojure
(def html (html-snippet "<h1>En overskrift</h1>"))

(enlive/defsnippet html [:h1] "<h2>En ny overskrift</h2>") ; => <h2>En ny overskrift</h2>
```

For å legge til et nytt element, kan du bruke funksjonen `enlive/append`:

```Clojure
(def html (html-snippet "<p>Hei, verden!</p>"))

(enlive/append html [:p] "<span>Velkommen!</span>") ; => <p>Hei, verden! <span>Velkommen!</span></p>
```

Enlive har også støtte for å hente informasjon fra HTML-tabeller og skjemaer, samt å utnytte CSS-selektorer for mer fleksibel henting av data.

## Se også

- Enlive-dokumentasjon: https://github.com/cgrand/enlive
- Lær mer om HTML-parsing: https://developer.mozilla.org/en-US/docs/Web/Guide/HTML/Introduction
- Utforsk andre Clojure-biblioteker: https://clojars.org/