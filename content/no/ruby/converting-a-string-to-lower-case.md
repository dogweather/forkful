---
title:                "Konvertere en streng til små bokstaver."
html_title:           "Ruby: Konvertere en streng til små bokstaver."
simple_title:         "Konvertere en streng til små bokstaver."
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvis du noen gang har jobbet med tekstbehandling i Ruby, har du kanskje støtt på et tilfelle der du må konvertere en streng til små bokstaver. Dette kan være nyttig for å standardisere data eller for å gjøre de små bokstavene mer leselige.

## Hvordan

Konvertering av strenger til små bokstaver er enkelt med en innebygd Ruby metode kalt `downcase`. Her er et eksempel:

```Ruby
name = "John Doe"
puts name.downcase
```
Output:

```
john doe
```
Som du kan se, er alle bokstavene i strengen "John Doe" nå konvertert til små bokstaver.

Det er også mulig å bruke denne metoden på en mer kompleks streng som inneholder tall eller spesialtegn. Her er et annet eksempel:

```Ruby
string = "P4$$w0rD!"
puts string.downcase
```
Output:

```
p4$$w0rd!
```

## Dypdykk

Nå som du vet hvordan du kan konvertere en streng til små bokstaver, la oss ta en nærmere titt på hvordan denne metoden fungerer. `downcase` funksjonen bruker Unicode tabeller for å vite hvilken bokstav som skal konverteres til små bokstaver. Dette betyr at hvis du jobber med et språk som bruker spesielle tegn eller diakritiske tegn, vil disse også bli konvertert til små bokstaver.

Det er også verdt å nevne at `downcase` ikke endrer selve variabelen som metoden blir brukt på, men returnerer heller en ny streng. Dette betyr at vi kan lagre det konverterte resultatet i en ny variabel eller bare bruke det direkte i utskriften.

## Se også

- [Ruby String Documentation](https://ruby-doc.org/core-2.7.2/String.html)
- [Unicode Code Charts](https://www.unicode.org/charts/)