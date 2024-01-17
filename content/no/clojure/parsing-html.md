---
title:                "Analysering av HTML"
html_title:           "Clojure: Analysering av HTML"
simple_title:         "Analysering av HTML"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/parsing-html.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Parsing HTML handler om å tolke og ekstrahere data fra HTML-kode. Dette er viktig fordi det er hvordan vi får informasjon til å se ut og fungere som vi ønsker på nettsider.

## Slik gjør du:
```Clojure
(defn parse-html [html]
  (if (nil? html)
    "Tom input"
    (str "Parsed HTML: " (first (re-seq #"\<[^>]*\>" html)))))
```
Dette kodeeksempelet viser en funksjon som tar HTML-kode som input og returnerer en streng med den første taggen som blir funnet. Et eksempel på hvordan koden ville kjøre:

```Clojure
(parse-html "<h1>Hello world!</h1>")
```
Output:
```Clojure
"Parsed HTML: <h1>"
```

## Dybdeutdypning:

### Historisk kontekst:
Parsing HTML har vært en nødvendighet siden begynnelsen av internett, da det var behov for å tolke og presentere informasjon på en standardisert måte. HTML-standarden har utviklet seg over tid, og parsing-teknikker har måttet tilpasse seg forandringer og utvidelser i standarden.

### Alternativer:
Det finnes flere forskjellige verktøy og biblioteker for å parse HTML på forskjellige programmeringsspråk. Blant annet finnes det også alternative metoder som bruker DOM-modeller eller CSS-selektorer istedenfor regex for å ekstrahere data.

### Implementasjonsdetaljer:
I Clojure er det vanlig å bruke regex til å parse HTML, da regex er en effektiv og uttrykksfull måte å finne mønstre i tekst på. Det finnes også spesifikke biblioteker som er laget for å parse HTML i Clojure, som for eksempel Enlive og Hiccup.

## Se også:
- Dokumentasjon for Clojure regex: https://clojure.org/guides/learn/regular_syntax
- Enlive biblioteket for parsing av HTML i Clojure: https://github.com/cgrand/enlive
- Hiccup biblioteket for generering av HTML i Clojure: https://github.com/weavejester/hiccup