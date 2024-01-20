---
title:                "Analyse av HTML"
date:                  2024-01-20T15:30:45.295705-07:00
html_title:           "Arduino: Analyse av HTML"
simple_title:         "Analyse av HTML"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why?
Parsing HTML betyr å tolke og konvertere HTML-kode slik at data kan hentetes og manipuleres. Programmerere gjør dette for å skrape nettsider, trekke ut informasjon, eller migrere innhold til en annen plattform.

## How to:
I Clojure kan du bruke `enlive` biblioteket for å enkelt parse HTML.

```Clojure
(require '[net.cgrand.enlive-html :as html])

(defn fetch-titles [html-content]
  (html/select (html/html-resource (java.io/StringReader. html-content))
               [:title]))

(let [html-content "<html><head><title>Hei, Norge!</title></head><body></body></html>"]
  (println (:content (first (fetch-titles html-content)))))
```

Forventet resultat:

```
"Hei, Norge!"
```
Dette eksemplet henter ut innholdet i `<title>` tagger fra HTML-strengen.

## Deep Dive
HTML-parsing har en lang historie, tett knyttet til utviklingen av internett og behovet for datahøsting. I Clojure-miljøet har `enlive` lenge vært et sterk verktøy for parsing, men alternativer som `hickory` og `jsoup` (via Java interoperabilitet) finnes også.

Ved å benytte `enlive` kan du utnytte dens DSL (domain-specific language) for å peke på spesifikke HTML-elementer du er interessert i. Dette krever forståelse av CSS-selektorer og grundig kunnskap om HTML-strukturen du jobber med.

Interessant nok er parsing av HTML en ikke-triviell oppgave på grunn av "uren" HTML som ofte finnes i det virkelige liv, noe som fører til at parseren må være både tolerant og intelligent for å håndtere varierende kvaliteter på HTML-koden.

## See Also
- Enlive GitHub Repo: https://github.com/cgrand/enlive
- ClojureDocs Enlive Examples: https://clojuredocs.org/net.cgrand.enlive-html
- jsoup: https://jsoup.org/ (for Java interoperabilitet)