---
title:    "Ruby: Rolle på språkallThis is the title of an article on computer programming: Sammenligner to datoer."
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/ruby/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Hvorfor
Har du noen gang vært i situasjonen der du trenger å sammenligne to datoer? Kanskje du lurer på om en bestemt dato er tidligere eller senere enn en annen dato, eller om de to datoene faller innenfor samme tidsperiode. Uansett årsaken, er det viktig å kunne sammenligne datoer i Ruby-programmering for å få nøyaktig informasjon. I denne bloggposten vil vi se på hvordan man kan sammenligne to datoer i Ruby og hvorfor det kan være nyttig.

## Hvordan gjøre det
Det første trinnet er å definere de to datoene som du ønsker å sammenligne. Dette kan gjøres ved å bruke Ruby Date-objekter. La oss si at vi har to datoer, 20. mai 2021 og 15. juni 2021. Vi vil sammenligne disse to datoene for å se hvilken som er tidligere.

```Ruby
require 'date'

date_one = Date.new(2021,5,20)
date_two = Date.new(2021,6,15)

if date_one > date_two
    puts "Dato en kommer senere enn dato to."
elsif date_one < date_two
    puts "Dato en kommer før dato to."
else
    puts "Datoene er like."
end
```

Output av dette vil bli "Dato en kommer før dato to." Dette skyldes at 15. juni kommer etter 20. mai.

En annen nyttig funksjon er å se om to datoer faller innenfor samme tidsperiode. Dette kan gjøres ved å bruke "between?" metoden på Date-objekter.

```Ruby
require 'date'

date_one = Date.new(2021,4,1)
date_two = Date.new(2021,4,15)
date_three = Date.new(2021,4,8)

if date_three.between?(date_one, date_two)
    puts "Dato tre er innenfor tidsperioden mellom dato en og dato to."
end
```

Output av dette vil bli "Dato tre er innenfor tidsperioden mellom dato en og dato to."

## Dypdykk
Når man sammenligner datoer, er det viktig å forstå at man også sammenligner tid og tidssoner. Dette kan føre til uventede resultater hvis man ikke er oppmerksom på dette. For eksempel, hvis du sammenligner datoer med forskjellige tidssoner, vil Ruby automatisk konvertere datoene til samme tidssone før sammenligningen skjer. Det er også viktig å merke seg at Ruby Date-objekter ikke inkluderer tid, bare datoer.

En annen ting å være oppmerksom på er at når man bruker "between?" metoden, vil den inkludere både startdatoen og sluttdatoen i tidsperioden man sammenligner med. Hvis du kun ønsker å sjekke om en dato faller mellom to datoer og ikke inkluderer start- og sluttdatoen, kan man bruke "cover?" metoden i stedet.

## Se også
- [Ruby Date dokumentasjon](https://ruby-doc.org/stdlib-3.0.0/libdoc/date/rdoc/Date.html)
- [Sammenligne datoer i Ruby](https://www.rubyguides.com/2018/08/ruby-date/)

Takk for at du leste denne bloggposten om hvordan man sammenligner datoer i Ruby! Vi håper den har vært nyttig for deg i din programmeringsreise. Ha det gøy med å eksperimentere med å sammenligne datoer i dine egne kodeprosjekter. Lykke til!