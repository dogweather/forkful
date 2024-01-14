---
title:                "Ruby: Å finne lengden av en streng"
simple_title:         "Å finne lengden av en streng"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

##Hvorfor

Det er en vanlig oppgave i programmering å finne lengden på en tekststreng. Dette kan være nyttig når du arbeider med tekstbehandling, eller hvis du trenger å begrense eller manipulere en tekststreng. I denne bloggposten skal vi se på hvordan du enkelt kan finne lengden på en tekststreng i Ruby.

##Slik Gjør Du

For å finne lengden på en tekststreng i Ruby kan du bruke metoden `length`, som returnerer antall tegn i strengen. La oss se på et eksempel:

```ruby
tekst = "Hei, jeg heter Ruby!"
puts tekst.length
```
Dette vil gi følgende utskrift:
```
18
```
Hvis du vil finne lengden på en tekststreng som er skrevet inn av brukeren, kan du bruke metoden `gets` og kombinere den med `length`. Se på dette eksempelet, der vi ber brukeren om å skrive inn navnet sitt og deretter finner lengden på strengen:
```ruby
puts "Skriv inn ditt navn:"
navn = gets.chomp
puts "Ditt navn har #{navn.length} bokstaver."
```
Hvis brukeren skriver inn "Hanne", vil utskriften bli:
```
Ditt navn har 5 bokstaver.
```

##Dypdykk

For å forstå hvordan `length`-metoden fungerer, er det nyttig med en dypere forståelse av hvordan Ruby behandler tekststrenger. For det første, i Ruby er tekststrenger objekter og har derfor ulike egenskaper og metoder som kan brukes på dem. Metoden `length` er en av disse metodene.

Når du kaller `length` på en tekststreng, teller Ruby antall tegn i strengen og returnerer dette tallet. Det er viktig å huske at mellomrom også regnes med som et tegn, i tillegg til bokstaver og spesialtegn.

Det finnes også andre metoder for å finne lengden på en tekststreng i Ruby, som for eksempel `size`og `count`. Disse gjør i utgangspunktet det samme som `length`, men det kan være nyttig å utforske dem nærmere for å finne ut hva som passer best i ulike situasjoner.

##Se Også

* [Ruby dokumentasjon for String-class](https://ruby-doc.org/core-2.7.0/String.html)
* [RubyGuides - String Length](https://www.rubyguides.com/ruby-string-length/)