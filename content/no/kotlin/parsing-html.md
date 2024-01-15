---
title:                "Analysering av HTML"
html_title:           "Kotlin: Analysering av HTML"
simple_title:         "Analysering av HTML"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/parsing-html.md"
---

{{< edit_this_page >}}

## Hvorfor

Lurer du på hvordan du kan trekke ut spesifikke deler av en nettsides innhold? Da er parsing av HTML det riktige verktøyet for deg! Ved å bruke Kotlin kan du enkelt utnytte denne funksjonen og få tilgang til dataen du trenger.

## Slik gjør du det

For å parse HTML i Kotlin trenger du først å importere et bibliotek for å få tilgang til funksjonene for parsing. Et populært bibliotek er Jsoup, som kan importeres i Kotlin ved å legge til følgende linje i gradle:

```Kotlin
implementation 'org.jsoup:jsoup:1.13.1'
```

Nå kan du begynne å bruke funksjonene for parsing. La oss si at du vil hente ut tittelen på en nettside, da kan du bruke følgende kode:

```Kotlin
val doc = Jsoup.connect("https://www.wikipedia.org").get()
val title = doc.title()
println(title)
```

Dette vil gi følgende output:

```Kotlin
Wikipedia
```

Du kan også hente ut innhold fra en bestemt tag på nettsiden ved å bruke funksjonen `getElementById`. For eksempel, hvis du vil ha innholdet fra `<h1>` taggen på Wikipedia-siden, kan du bruke følgende kode:

```Kotlin
val doc = Jsoup.connect("https://www.wikipedia.org").get()
val header = doc.getElementById("mp-topbanner")
println(header.text())
```

Dette vil gi output av tittelen på Wikipedia.

## Dykk dypere

Å parse HTML kan også være nyttig hvis du vil hente ut spesifikke data fra nettsiden, som for eksempel informasjon om en bok fra en nettbokhandel. Ved å bruke funksjonene for parsing kan du enkelt hente ut tittel, forfatter, pris og annen relevant informasjon fra nettsiden og bruke det til din fordel.

Det finnes også andre biblioteker for å parse HTML i Kotlin, som for eksempel Jsoup-ktx, som gir en mer Kotlin-vennlig syntaks. Utforsk forskjellige biblioteker og finn det som passer best for ditt prosjekt.

## Se også

- [Jsoup biblioteket](https://jsoup.org/)
- [Kotlin kodeeksempler for parsing av HTML](https://github.com/jsoup/jsoup)

Med dette verktøyet i verktøykassen vil du nå være i stand til å enkelt hente ut data fra nettsider ved hjelp av Kotlin. Utforsk mulighetene og ha det gøy med å utforske og manipulere HTML. Lykke til!