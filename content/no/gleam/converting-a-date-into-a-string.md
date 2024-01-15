---
title:                "Konvertering av dato til streng"
html_title:           "Gleam: Konvertering av dato til streng"
simple_title:         "Konvertering av dato til streng"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Hva er en av de vanligste operasjonene som trengs når man jobber med data? Konvertering av en dato til en streng. Her vil jeg vise deg hvordan du kan gjøre dette enkelt og effektivt ved å bruke Gleam!

## Hvordan

Konvertering av en dato til en streng er enkelt med Gleam. Du kan bruke den innebygde funksjonen `Date.to_string` som tar inn en dato og returnerer en streng i formatet "YYYY-MM-DD". Her er et eksempel på hvordan du kan bruke denne funksjonen:

```Gleam
let dato = Date.new(2021, 8, 4)
let dato_string = Date.to_string(dato)

// Utskrift: "2021-08-04"
Debug.log(dato_string)
```

Som du kan se, tar `Date.new` inn år, måned og dag som argumenter for å lage en dato. Deretter kan du bruke `Date.to_string` for å konvertere datoen til en streng. Dette er en rask og enkel måte å håndtere datoer på i Gleam.

## Dypdykk

Hvis du ønsker å konvertere en dato til en streng med et annet format enn "YYYY-MM-DD", kan du bruke funksjonen `Date.format` i stedet. Denne funksjonen tar inn en dato, et format og en språkkode, og returnerer en streng i det gitte formatet og språket. Her er et eksempel på hvordan du kan bruke denne funksjonen:

```Gleam
let dato = Date.new(2021, 8, 4)

// Utskrift: "Onsdag 04. august 2021"
Date.format(dato, "EEEE dd. MMMM yyyy", "no")
```

Som du kan se, er `Date.format` fleksibel og lar deg konvertere datoen til en streng i ønsket format og språk. Du kan sjekke [Gleam dokumentasjonen](https://gleam.run/documentation/) for et fullstendig utvalg av format og språkkoder som støttes.

## Se også

- [Gleam dokumentasjon](https://gleam.run/documentation/)
- [Hvordan arbeide med datoer i Gleam](https://medium.com/@gleamlang/dates-in-gleam-b7dba21b95b7)
- [Gleam fellesskap på Discord](https://discord.gg/gleam)