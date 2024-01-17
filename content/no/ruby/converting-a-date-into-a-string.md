---
title:                "Konvertere en dato til en streng"
html_title:           "Ruby: Konvertere en dato til en streng"
simple_title:         "Konvertere en dato til en streng"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Konvertering av en dato til en streng er en vanlig operasjon i programmering som lar utviklere representere og manipulere datoer på en enklere måte. Ved å konvertere en dato til en streng, kan vi enkelt formatere og vise datoer i forskjellige formater, som for eksempel å vise dem som månedsnavn eller numeriske verdier.

## Hvordan?

```Ruby
# Konverter en dato til en streng med standardformat
date = Date.new(2020, 8, 24)
date.to_s #=> "2020-08-24"
```

```Ruby
# Formatere dato med månedsnavn
date.strftime("%d %B %Y") #=> "24 August 2020"
```

## Dypdykk

Datoformatering har vært en utfordring i programmering siden de tidligste dagene. Før utviklingen av objektorienterte programmereringsspråk som Ruby, måtte utviklere håndtere datofunksjoner manuelt. Men med utviklingen av Ruby on Rails, har konvertering av datoer til strenger blitt mye enklere og mer intuitivt.

Det finnes også alternativer til å bruke `to_s`-metoden, som for eksempel å bruke `strftime`-metoden som lar oss definere forskjellige datoformater ved hjelp av formateringsstringer.

En annen viktig ting å merke seg ved konvertering av datoer til strenger, er at det kan påvirke ytelsen på applikasjonen. Dette er spesielt viktig når man manipulerer store mengder av datoer.

## Se også

- [Date class documentation](https://ruby-doc.org/stdlib/libdoc/date/rdoc/Date.html)
- [Ruby on Rails documentation](https://guides.rubyonrails.org/active_support_core_extensions.html#to-format)
- [Eksperimenter med datoformatering på Repl.it](https://repl.it/@Ruby/Date-to-String)