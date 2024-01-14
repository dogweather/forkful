---
title:                "Ruby: Å bruke regulære uttrykk"
programming_language: "Ruby"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Hvorfor

Regulære uttrykk er en kraftig verktøy for å søke og manipulere tekst i et programmeringsspråk som Ruby. De lar deg finne og behandle tekstmønstre på en effektiv måte, noe som kan være svært nyttig i dataanalyse, tekstbehandling og mer.

## Hvordan

For å bruke regulære uttrykk i Ruby, må du først importere "regexp" biblioteket. Deretter kan du definere uttrykket ditt innenfor et "```Ruby ... ```" kodeblokk som følger:

```Ruby
my_string = "Dette er en tekststreng som inneholder tallene 123"

my_regex = /(\d+)/ # (d+) angir at vi ønsker å finne alle tallene i strengen

matches = my_string.scan(my_regex) # gir oss en liste over alle tallene som matchet uttrykket vårt

puts matches # output: ["123"]
```

## Dypdykk

Det er mange forskjellige spesifikke uttrykk og metoder du kan bruke med regulære uttrykk i Ruby. For eksempel kan du bruke "match" metoden til å finne et spesifikt mønster i en streng, eller "sub" metoden for å erstatte deler av en streng med annen tekst. Det er også mulig å bruke flagg for å endre hvordan uttrykket tolkes, for eksempel for å ignorere store og små bokstaver eller begynnelsen og slutten på en linje.

## Se Også

- [En oversikt over regulære uttrykk i Ruby](https://www3.ntu.edu.sg/home/ehchua/programming/howto/Regexe.html)
- [Ruby sin offisielle dokumentasjon for regexp biblioteket](https://ruby-doc.org/core-3.0.0/Regexp.html)
- [En tutorial for å lære mer om regulære uttrykk i Ruby](https://www.youtube.com/watch?v=sa-TUpSx1JA)