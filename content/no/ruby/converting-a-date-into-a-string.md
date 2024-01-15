---
title:                "Omdannelse av dato til tekst"
html_title:           "Ruby: Omdannelse av dato til tekst"
simple_title:         "Omdannelse av dato til tekst"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor
Å konvertere en dato til en streng er en viktig del av å programmere i Ruby, spesielt når man jobber med brukergrensesnitt eller data som må vises i en bestemt format. Det gir deg også fleksibilitet til å formatere datoer på en måte som er mer forståelig for brukeren.

## Hvordan gjøre det
```Ruby
# Konverter dato til en streng
date = Date.new(2021,9,30)
string_date = date.to_s
puts string_date
# Output: "2021-09-30"

# Formatere dato i en annen måte
date = Date.new(2021,9,30)
string_date = date.strftime("%d.%m.%Y")
puts string_date
# Output: "30.09.2021"
```

Det første eksempelet viser hvordan man enkelt kan konvertere en dato til en standard streng ved hjelp av `to_s` metoden. Det andre eksempelet viser hvordan man kan bruke `strftime` metoden til å formatere datoen etter ønsket format, ved hjelp av spesielle formateringstegn som `%d` for dag, `%m` for måned og `%Y` for år.

## Deep Dive
Når man konverterer en dato til en streng, bruker Ruby `to_s` metoden som kalder `to_s` metoden i `Date` klassen. Dette resulterer i en standard streng på formatet `åååå-mm-dd`, med mindre du formaterer det på en annen måte.

`strftime` metoden gir deg enda mer kontroll over formateringen av datoen, og lar deg bruke forskjellige formateringstegn for å tilpasse det etter dine behov. Her er noen av de mest brukte formateringstegnene:

- `%a` - Forkortet ukedag (f.eks. Mon, Tue, osv.)
- `%A` - Full ukedag (f.eks. Monday, Tuesday, osv.)
- `%b` - Forkortet månednavn (f.eks. Jan, Feb, osv.)
- `%B` - Full månednavn (f.eks. January, February, osv.)
- `%d` - Dag i måneden, med ledende null (f.eks. 01, 02, osv.)
- `%m` - Måned i år, med ledende null (f.eks. 01, 02, osv.)
- `%Y` - Fullt årstall (f.eks. 2021, 2022, osv.)

Å konvertere en dato til en streng kan også være nyttig når man skal lagre datoer i en database eller arbeide med datoer i forskjellige tidssoner.

## Se også
- [Date Class in Ruby](https://ruby-doc.org/stdlib-3.0.1/libdoc/date/rdoc/Date.html)
- [strftime method in Ruby](https://ruby-doc.org/core-3.0.1/Time.html#method-i-strftime)