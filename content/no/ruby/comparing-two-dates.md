---
title:                "Sammenligner to datoer"
html_title:           "Ruby: Sammenligner to datoer"
simple_title:         "Sammenligner to datoer"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Hvorfor

"Datoer" er en vanlig del av programmering, og det er ofte behov for å sammenligne to bestemte datoer. Dette kan være nyttig for å sjekke om en hendelse har skjedd før eller etter en annen hendelse, eller for å sortere data basert på datoer. Ved hjelp av Ruby kan du enkelt sammenligne to datoer og få nøyaktig informasjon om dette.

## Slik gjør du det

For å sammenligne to datoer i Ruby, kan du bruke metoden `Date#<=>`. Denne metoden tar inn to datoer som argumenter og returnerer en verdi som indikerer deres forhold til hverandre.

```Ruby
dato1 = Date.new(2020, 07, 15)
dato2 = Date.new(2021, 03, 10)

dato1 <=> dato2 #returnerer -1 siden dato1 kommer før dato2
```

I dette eksempelet vil `Date#<=>` returnere -1 siden dato1 kommer før dato2. Hvis dato1 hadde vært etter dato2, ville det returnert 1. Hvis datoene er like, returnerer det 0.

Du kan også bruke de vanlige sammenligningsoperatorene (`<`, `>`, `==`) for å sammenligne datoer.

```Ruby
dato1 < dato2 #returnerer true
dato1 > dato2 #returnerer false
dato1 == dato2 #returnerer false
```

For å finne ut hvor mange dager det er mellom to datoer, kan du bruke metoden `Date#between?`. Denne metoden tar inn to datoer som argumenter og returnerer et antall dager mellom dem.

```Ruby
  dato1 = Date.new(2020, 07, 15)
  dato2 = Date.new(2021, 03, 10)

  (dato2 - dato1).between?(85, 87) #returnerer true siden det er 86 dager mellom datoene
```

## Dypdykk

Når du sammenligner to datoer i Ruby, sammenlignes ikke bare dagene, men også månedene og årene. Dette betyr at hvis to datoer faller på samme dag, men i forskjellig år, vil de fortsatt bli vurdert som forskjellige når de sammenlignes.

I tillegg kan du også konvertere datoer til andre formater, som strenger eller Unix-tidsstempel, ved hjelp av tilsvarende metoder.

For mer detaljert informasjon om datoer i Ruby, kan du sjekke ut dokumentasjonen på Ruby's Date-klasse.

## Se også

- [Datoer i Ruby dokumentasjon](https://ruby-doc.org/stdlib-2.7.2/libdoc/date/rdoc/Date.html)
- [Sammenligning av datoer i Ruby screencast](https://youtu.be/_Yg5iRzNrxQ)
- [Sammenligne datoer i Ruby blogginnlegg](https://www.rubyguides.com/2017/10/ruby-date-compare/)