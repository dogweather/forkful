---
title:    "Gleam: Konvertering av en streng til små bokstaver"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/gleam/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor skulle noen ønske å konvertere en streng til små bokstaver? Vel, det er et viktig skritt i å formatere tekst og data for å gjøre det mer leselig og konsistent. Dette kan være spesielt nyttig når man jobber med store mengder tekst og ønsker å unngå forvirring og feil.

## Hvordan

For å konvertere en streng til små bokstaver i Gleam, kan du bruke funksjonen `String.to_lower_case()`. Denne funksjonen tar inn en streng som argument og returnerer en ny streng med alle bokstavene konvertert til små bokstaver. Her er et eksempel:

```Gleam
let string = "EN STOR STRENG"
let converted_string = String.to_lower_case(string)

IO.print(converted_string) // vil skrive ut "en stor streng"
```

Som du ser, kan du enkelt bruke funksjonen `IO.print()` for å se den konverterte strengen i aksjon. Denne funksjonen er spesielt nyttig når du jobber med Gleam i et terminalprogram eller annen interaktiv miljø.

## Dypdykk

Nå som du vet hvordan du kan konvertere en streng til små bokstaver i Gleam, la oss ta et dypere dykk inn i hva som skjer bak kulissene. Når du bruker `String.to_lower_case()`-funksjonen, vil Gleam ta hver enkelt karakter i strengen og sammenligne den med en liste over store bokstaver. Hvis en match blir funnet, vil bokstaven bli konvertert til en tilsvarende liten bokstav. Dette gjøres med en kombinasjon av `String.split()` og `List.map()` funksjoner.

En ting å være oppmerksom på er at denne funksjonen bare vil konvertere standard ASCII-alfabetet. Dette betyr at bokstaver fra andre språk og spesialtegn ikke vil bli konvertert på samme måte. Hvis du trenger å konvertere strenger med slike tegn, bør du utforske andre funksjoner og metoder.

## Se Også

- [Gleam Dokumentasjon: String Module](https://gleam.run/documentation/stdlib/string/)
- [Gleam Dokumentasjon: IO Module](https://gleam.run/documentation/stdlib/io/)
- [Gleam Eksempler: Konvertering av Strenger til Små Bokstaver](https://github.com/gleam-lang/gleam/blob/master/examples/string_to_lower_case.gleam)