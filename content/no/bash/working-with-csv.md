---
title:                "Å jobbe med csv"
html_title:           "Bash: Å jobbe med csv"
simple_title:         "Å jobbe med csv"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/working-with-csv.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Arbeid med CSV er en måte for programmerere å organisere og manipulere data som er lagret i CSV (comma-separated values) format. Dette kan være nyttig når du jobber med store datasett eller når du må konvertere data mellom forskjellige programmer eller systemer.

## Slik gjør du det:

For å jobbe med CSV-filer i Bash, kan du bruke kommandolinjeverktøyet "csvtool" som finnes i de fleste Linux-distribusjoner. Her er et eksempel på hvordan du kan konvertere en CSV-fil til et HTML-dokument:

```Bash
csvtool readable input.csv > output.html 
```
Dette vil lage en HTML-fil med tabellformattet data fra CSV-filen.

## Dykk dypere:

CSV-formatet ble opprinnelig utviklet i 1972 og har vært et populært format for å lagre tabellbaserte data. Alternativer til CSV inkluderer XML og JSON. Når du jobber med CSV i Bash, kan du bruke kommandolinjetrafikk for å manipulere filene, som å legge til eller fjerne kolonner eller rader, og filtrere data basert på bestemte kriterier.

## Se også:

- [Official Bash documentation on csvtool](http://tldp.org/LDP/abs/html/csvtool.html)
- [Tutorial on working with CSV in Bash](https://www.linode.com/docs/tools-reference/tools/how-to-use-csvtool/)
- [Comparison of CSV, XML, and JSON formats](https://medium.com/better-programming/csv-vs-json-vs-xml-which-is-the-best-response-format-833082bbd8d5)