---
title:                "Interpolering av en streng"
html_title:           "Bash: Interpolering av en streng"
simple_title:         "Interpolering av en streng"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/interpolating-a-string.md"
---

{{< edit_this_page >}}

---

## Hva & Hvorfor?
Interpolering av en streng betyr å injisere variabler direkte i strengen. Programmerere gjør dette for å lette verdiinnsamling og skrive renere kode.

## Hvordan gjøre det:
I Gleam, bruk `{}` for å interpolere string. Sjekk koden nedenfor:

```Gleam
let navn = "Ola"
let hilsen = "Hei, {}"
io.println(hilsen.format([navn])) // Output: Hei, Ola
```

Her er et eksempel til for å vise multi-interpolering:

```Gleam
let x = 10
let y = 20
io.println("Vi adderer {} og {} for å få {}.".format([x, y, x+y])) // Output: Vi adderer 10 og 20 for å få 30.
```

## Dyp Dykk
String interpolering startet som en funksjon i RPG og andre programmeringsspråk i 1960-tallet. Det gir en mer lesbar og kompakt måte å manipulere strenger på. Mens noen programmeringsspråk som Perl og Python innebygger det i selve språket, krever Gleam en mer uttrykkelig metode ved bruk av `.format()` -funksjonen.

Alternativene til string interpolering inkluderer bruk av tradisjonelle strengmanipulasjonsfunksjoner som konkatenasjon. Men det er en mindre ønskelig tilnærming på grunn av deres verbose natur.

Implementeringsdetaljer: Gleam bruker en liste med variabler i `.format()` -funksjonen som deretter innebygges i strengverdien på `{}` -merkene.

## Se Også
For mer informasjon, se disse kildene:
- [Gleam String Dokumentasjon](https://gleam.run/book/tour/strings.html)
- [Historien om String Interpolering](https://en.wikipedia.org/wiki/String_interpolation)

---

Husk, øvelse gjør mester! Prøv selv på forskjellige varianter av strenginterpolering i Gleam.