---
title:    "Ruby: Sammenligning av to datoer"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Hvorfor

Å sammenligne to datoer er en viktig del av programmering, spesielt når man jobber med tidssensitive applikasjoner eller funksjoner. Det kan hjelpe deg å sortere og filtrere data, samt sjekke om en dato har passert eller ikke. Å forstå hvordan man sammenligner datoer kan gjøre programmeringen din mer effektiv og nøyaktig.

## Slik gjør du det

Å sammenligne to datoer i Ruby kan gjøres ved å bruke den innebygde `Date`-klassen. La oss si at vi har to variabler `date1` og `date2` som inneholder to forskjellige datoer. For å sjekke om `date1` kommer før `date2` kan vi bruke `date1 < date2` og for å sjekke om de er like, kan vi bruke `date1 == date2`.

```Ruby
date1 = Date.new(2021, 6, 1)
date2 = Date.new(2021, 7, 1)

puts date1 < date2 # Output: true
puts date1 == date2 # Output: false
```

Det er også mulig å bruke `Time`-klassen for å sammenligne datoer basert på klokkeslettet. For eksempel, hvis vi vil sjekke om `date1` og `date2` er samme dag, men `date1` er tidligere på dagen, kan vi bruke `date1.localtime < date2.localtime`.

## Dypdykk

Når du sammenligner datoer er det viktig å ha en god forståelse av hvordan Ruby håndterer datoer og klokkeslett. Det er også viktig å huske at datoer i Ruby er objekter og kan derfor brukes med metoder, som for eksempel `strftime` for å formatere datoen på en bestemt måte.

Du kan også bruke metoden `between?` for å sjekke om en dato faller mellom to andre datoer. For eksempel, hvis vi har en variabel `date3` som inneholder datoen 15. juni 2021, kan vi bruke `date3.between?(date1, date2)` for å sjekke om datoen ligger mellom `date1` og `date2`.

## Se også

- [Ruby Date Class Documentation](https://ruby-doc.org/stdlib-2.7.2/libdoc/date/rdoc/Date.html)
- [Ruby Time Class Documentation](https://ruby-doc.org/core-2.7.2/Time.html)
- [Ruby Comparison Operators](https://medium.com/rx11/working-with-comparison-operators-in-ruby-b06d48f36058)