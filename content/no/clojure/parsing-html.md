---
title:                "Analysering av html"
html_title:           "C#: Analysering av html"
simple_title:         "Analysering av html"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/parsing-html.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

HTML-parsing er prosessen med å analysere HTML-kode for å opprette objekter som kan manipuleres og brukes i dine programmer. Dette gjør det mulig for programmerere å hente ut data, manipulere strukturer, eller programmere interaksjoner i websider.

## Hvordan gjøre det:

Clojure har en fantastisk bibliotek for å parse HTML - Enlive. La oss se hvordan det fungerer. For eksempel, tenk at vi har en enkel HTML-fil som vi vil jobbe med:

```html
<!DOCTYPE html>
<html>
    <body> 
        <h1>Hei Clojure!</h1>
        <p>Velkommen til programmeringsverden!</p>
    </body>
</html>
```
La oss analysere den med Enlive. For det første, la oss legge inn Enlive avhengigheten i `project.clj`:

```clojure
(defproject parsing-demo "0.1.0-SNAPSHOT"
            :dependencies [[org.clojure/clojure "1.10.1"]
                           [net.cgrand/enlive "1.1.6"]])
```
Og her skal vi parse HTML-koden:

```clojure
(require '[net.cgrand.enlive-html :as html])

(defn parse-html []
    (let [htmldoc (html/html-resource (java.io.StringReader. "<!DOCTYPE html> <html> <body> <h1>Hei Clojure!</h1><p>Velkommen til programmeringsverden!</p></body></html>"))]
        (println htmldoc)))
```
Kjør funksjonen `parse-html`, så får du utskriften:

```clojure
{:tag :html, :attrs nil, :content ({:tag :body, :attrs nil, :content...} )
```
## Deep Dive:

HTML-parsing har vært en viktig del av webutvikling siden begynnelsen, og det finnes ulike måter å bygge HTML-parser på. Når vi snakker om Clojure, har vi tilgang til flere biblioteker å velge mellom, ikke bare Enlive.

For eksempel, hickory er et annet populært bibliotek for HTML-parsing i Clojure. Det gir lignende funksjonalitet som Enlive, men med en litt annen syntaks og brukergrensesnitt.

Det er viktig å merke seg at parsing i seg selv ikke er nok for å jobbe effektivt med websider - du må også kunne manipulere og endre det parsede objektet. Med Clojure's funksjonelle programmeringsparadigme og rik datastrukturer, blir dette en ganske enkel oppgave.

## Se også:

- Dokumentasjon for Enlive: https://github.com/cgrand/enlive
- Hickory sitt repo: https://github.com/davidsantiago/hickory
- Clojure sin guide på web-scraping: https://clojure-cookbook.com/posts_output/web_scraping_with_enlive.html
- HTML-parsing utført med Java: https://www.journaldev.com/2016/html-parsing-using-java-htmlunit