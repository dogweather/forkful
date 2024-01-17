---
title:                "Analysere HTML"
html_title:           "Swift: Analysere HTML"
simple_title:         "Analysere HTML"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/parsing-html.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Parsing av HTML er prosessen med å konvertere HTML-kode til en strukturert form som kan leses og tolkes av en datamaskin. Programmører bruker dette for å få tilgang til og manipulere data presentert på nettsider.

## Slik gjør du det:
For å utføre parsing av HTML i Swift, kan du bruke et tredjeparts bibliotek som SwiftSoup. Dette biblioteket lar deg hente data fra nettsider ved hjelp av URL og deretter navigere gjennom DOM-treet for å finne og hente ut ulike elementer. Her er et enkelt eksempel på hvordan dette kan gjøres:
```
SwiftSoup.parse(htmlString)?.select("h1").first()?.text()
```
Dette koden vil lese HTML-koden fra en streng og deretter hente ut innholdet av det første "h1"-elementet som tekst.

## Dypdykk:
Parsing av HTML har vært et viktig konsept siden utviklingen av World Wide Web. HTML ble opprinnelig utviklet som et tolkbart språk for å presentere dokumenter på en enhetlig måte, men har utviklet seg til å også bli brukt som en datalagerstruktur for å representere informasjon på nettsider. Alternativene for å utføre parsing av HTML i Swift inkluderer også bibliotek som Kanna og AEXML. Implementeringen av parsing av HTML i Swift handler om å bruke riktige biblioteker og å forstå hvordan man skal navigere gjennom DOM-treet.

## Se også:
- [SwiftSoup](https://github.com/scinfu/SwiftSoup)
- [Kanna](https://github.com/tid-kijyun/Kanna)
- [AEXML](https://github.com/tadija/AEXML)