---
title:                "Gjøre en streng stor"
html_title:           "Ruby: Gjøre en streng stor"
simple_title:         "Gjøre en streng stor"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hva & hvorfor?

Kapitalisering av en streng, på programmeringsspråket, betyr å gjøre den første bokstaven i strengen stor. Programmerere gjør dette for å forbedre lesbarheten og for brukergrensesnittring (GUI).

## Hvordan gjøre det:

I Ruby kan vi kapitalisere en streng ved å bruke `.capitalize` metode. Her er et enkelt eksempel:

```Ruby
streng = "hej verden"
puts streng.capitalize
```

Når du kjører koden ovenfor, vil du se denne utdataen: 

`Hej verden`

Om du vil gjøre første bokstav i hvert ord stort, bruk `.titleize` i stedet:

```Ruby
streng = "hej verden"
puts streng.titleize
```

Resultatet vil da se slik ut: 

`Hej Verden`

## Dypdykk

Historisk sett, stammer konseptet om å kapitalisere bokstavene fra typografibransjen. I programmeringens verden, hjelper denne funksjonen med å gjøre innholdet mer leselig og tiltalende.

Et alternativ til `.capitalize` og `.titleize` kan være å bruke metoden `.upcase` på den første bokstaven selv, men dette krever ekstra kode.

I Ruby, tilsvarer `.capitalize` metoden fungerer ved å bruke den innebygde `CHRUPCASE`-funksjonen til Ruby, noe som gjør den både enkel og effektiv.

## Se Også

For mer informasjon og ressurser angående dette emnet, kan du se på følgende lenker:

Rubys Dokumentasjon for Streng-metoder: https://ruby-doc.org/core-2.7.1/String.html
Detaljert blogginnlegg om strengkapitalisering: https://www.rubyguides.com/2018/10/ruby-capitalization/
Tutorial for Ruby streng-manipulering: https://www.tutorialspoint.com/ruby/ruby_strings.htm