---
title:                "Å finne lengden av en streng"
html_title:           "Ruby: Å finne lengden av en streng"
simple_title:         "Å finne lengden av en streng"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Det å finne lengden på en streng (string) er en grunnleggende ferdighet i Ruby og er nyttig for å manipulere tekst og data. Det kan også være nyttig når du jobber med looper og vil kontrollere antall iterasjoner.

## Hvordan gjøre det

For å finne lengden på en streng i Ruby, kan du bruke metoden `.length` eller `.size`. Her er et eksempel på hvordan du kan bruke disse metodene:

```Ruby
str = "Hei, verden!"

puts str.length
# Output: 12

puts str.size
# Output: 12
```

Som du kan se, returnerer begge metodene samme lengde for strengen vår. Det er fordi de i utgangspunktet gjør det samme. Du kan velge den du foretrekker å bruke.

Du kan også bruke `.count` metoden for å finne ut antall karakterer i en streng. Denne metoden tar et argument, for eksempel en bokstav, og returnerer antall forekomster av dette i strengen. La oss se på et eksempel:

```Ruby
str = "Welcome to Ruby!"

puts str.count("o")
# Output: 2
```

Her returnerer `.count` metoden antall "o"-er i strengen vår.

## Dypdykk

Hvis du vil utforske enda mer funksjonalitet rundt å finne lengden på en streng i Ruby, kan du sjekke ut dokumentasjonen for `String` klassen. Der finner du flere metoder som kan være nyttige for dette formålet, som for eksempel `.bytesize` som returnerer lengden på strengen i bytes, og `.empty?` som sjekker om strengen er tom.

Se også

- [String dokumentasjon](https://ruby-doc.org/core-2.7.1/String.html)
- [Ruby for nybegynnere](https://www.ruby-lang.org/no/documentation/quickstart/)