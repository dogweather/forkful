---
title:                "Søke og erstatte tekst"
html_title:           "Ruby: Søke og erstatte tekst"
simple_title:         "Søke og erstatte tekst"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Hva & Hvorfor?
Søking og erstatting av tekst er en vanlig oppgave for programmerere. Den innebærer å finne spesifikke tekstelementer og erstatte dem med nye verdier. Dette kan være nyttig for å automatisere repetitivt arbeid eller for å endre mange forekomster av en gittekst på en gang.

# Hvordan:
For å søke og erstatte tekst i Ruby kan du bruke metoden `gsub`. La oss si at vi har en streng som inneholder ordet "hallo" og vi vil erstatte det med "hei". Vi kan enkelt gjøre det ved å skrive følgende:
```
streng = "Hallo verden!"
puts streng.gsub("hallo", "hei")
```
Dette vil produsere følgende utdata:
```
Hei verden!
```
Som du kan se, erstattet `gsub` metoden alle forekomster av "hallo" med "hei" i vår streng.

# Dypdykk:
Søking og erstatting av tekst har eksistert i mange år og har blitt brukt i flere programmeringsspråk. En alternativ måte å gjøre det på i Ruby er ved å bruke metoden `sub` som bare erstatter den første forekomsten av den gitte teksten. Det finnes også andre programmeringsspråk som har innebygde funksjoner for å utføre denne oppgaven, for eksempel `replace()` i Python.

Implementerings-detaljer for søking og erstatting av tekst kan variere avhengig av programmeringsspråk og versjon. Det er viktig å sjekke dokumentasjonen for språket du jobber med for å sikre riktig bruk.

# Se også:
- Ruby sin offisielle dokumentasjon for `gsub` metoden: https://ruby-doc.org/core-2.7.1/String.html#method-i-gsub
- En guide til søking og erstatting i Ruby: https://www.rubyguides.com/2019/02/ruby-search-and-replace/