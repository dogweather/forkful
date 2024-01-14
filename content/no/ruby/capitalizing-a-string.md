---
title:    "Ruby: Kapitalisering av en streng"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Hvorfor

Har du noen gang lurt på hvorfor noen strenger ser ut som dette: "DENNE TEKSTEN ER SKIFTET TIL STORE BOKSTAVER"? Vel, det er fordi noen har brukt en funksjon i Ruby til å kapitalisere hele strengen. I denne bloggposten skal vi se nærmere på hvorfor man ville ønske å gjøre dette, og hvordan man kan gjøre det på en enkel måte.

## Hvordan gjøre det

For å kapitalisere en streng i Ruby, kan du bruke funksjonen `upcase`, som gjør om alle små bokstaver til store bokstaver:

```Ruby
puts "dette er en streng".upcase
```
Dette vil produsere følgende output:
```
DETTE ER EN STRENG
```
Hvis du bare ønsker å kapitalisere den første bokstaven i strengen, kan du bruke funksjonen `capitalize`:

```Ruby
puts "dette er en streng".capitalize
```
Dette vil produsere følgende output:
``` 
Dette er en streng
```

## Dypdykk

Å kapitalisere en streng kan være nyttig i mange sammenhenger. For eksempel, hvis du jobber med data som skal vises til brukere, kan det være mer leselig og visuelt tiltalende å ha hele strengen i store bokstaver. Eller kanskje du bare ønsker å gjøre en enkel formatering av data før du lagrer den i en database.

En ting å være oppmerksom på er at funksjonene `upcase` og `capitalize` endrer selve strengen som blir brukt som argument. Hvis du ønsker å lage en ny streng med den kapitaliserte versjonen, kan du bruke funksjonen `upcase!` og `capitalize!`:

```Ruby
streng = "dette er en streng"
kapitalisert_streng = streng.capitalize!

puts streng
puts kapitalisert_streng
```

Dette vil produsere følgende output:
``` 
Dette er en streng
Dette er en streng
```
I tillegg til disse funksjonene, finnes det også andre måter å kapitalisere en streng på i Ruby, som å bruke metoden `map`. Hvis du ønsker å utforske dette mer, kan du se på ressursene nedenfor.

## Se også
- [Ruby Dokumentasjon: String](https://ruby-doc.org/core-2.7.1/String.html)
- [How to Capitalize Strings in Ruby](https://www.rubyguides.com/2020/06/ruby-capitalization/)
- [String Manipulation in Ruby](https://www.digitalocean.com/community/tutorials/how-to-manipulate-strings-in-ruby)
- [Ruby on Rails Tutorial: Capitalizing Strings](https://www.railstutorial.org/book).