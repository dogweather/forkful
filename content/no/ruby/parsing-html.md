---
title:                "Analysering av html"
html_title:           "Ruby: Analysering av html"
simple_title:         "Analysering av html"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/parsing-html.md"
---

{{< edit_this_page >}}

## Hvorfor
Har du noen gang trengt å skrape informasjon fra et nettsted for å få dataene du ønsker? Da kan du kanskje få bruk for å parse HTML. Ved å bruke Ruby, kan du enkelt hente ut spesifikke data fra en nettside og bruke det til å lage dine egne applikasjoner eller automatisere oppgaver.

## Slik gjør du det
Det første du trenger er å installere et bibliotek som heter "Nokogiri" ved å kjøre følgende kommando i terminalen: 

```Ruby
gem install nokogiri
```

Når biblioteket er installert, kan du begynne å bruke det i ditt Ruby-program. Først importerer du biblioteket ved å legge til følgende linje øverst i programmet ditt:

```Ruby 
require 'nokogiri'
```

Deretter må du angi nettsiden du ønsker å parse, for eksempel:

```Ruby 
html = open("https://www.example.com")
```

Nå kan du begynne å finne og hente ut ønskede data fra nettsiden. La oss si at du ønsker å få tak i overskriften på siden. Da kan du bruke følgende kode:

```Ruby 
doc = Nokogiri::HTML(html)
title = doc.css("h1").text
puts title
```

I dette tilfellet bruker vi Nokogiri's "css" metode til å finne elementet med taggen "h1" og så henter vi ut teksten fra dette elementet og lagrer det i en variabel. Deretter skriver vi ut variabelen til konsollen og vil få utskrift av overskriften på nettsiden.

## Fordypning
Nokogiri er et kraftig bibliotek som gir mange verktøy for å manipulere HTML og XML. Du kan bruke ulike søkekriterier og filtre for å finne spesifikke elementer på en nettside. I tillegg kan du også hente ut attributter og data fra ulike tags. Det finnes mange gode ressurser på nettet som kan hjelpe deg med å lære mer om hvordan du bruker Nokogiri og parser HTML med Ruby.

## Se også
- [Nokogiri dokumentasjon](https://nokogiri.org)
- [Ruby sin offisielle nettside](https://www.ruby-lang.org/en/)
- [En enkel guide til å parse HTML med Ruby](https://medium.com/swiftly-swift/a-simple-guide-to-scraping-html-with-ruby-9b51f5cc6a14)