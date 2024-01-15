---
title:                "Konvertere en streng til små bokstaver"
html_title:           "Elixir: Konvertere en streng til små bokstaver"
simple_title:         "Konvertere en streng til små bokstaver"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hvorfor

Det er ofte et behov for å konvertere en streng til små bokstaver når du arbeider med tekstbehandling i programmering. Dette kan være for å opprettholde en konsistent formatering eller for å sammenligne strenger på en nøyaktig måte.

## Hvordan du gjør det

Her er et enkelt eksempel på hvordan du kan konvertere en streng til små bokstaver i Elixir:

```Elixir
string = "HEI, EKSPERIMENTER MED ELIXIR"
IO.puts String.downcase(string)
```

Output:
```Elixir
hei, eksperimenter med elixir
```

Som du kan se, bruker vi den innebygde funksjonen `downcase` fra `String`-modulen for å konvertere strengen til små bokstaver. Det er også verdt å merke seg at denne funksjonen ikke endrer den opprinnelige strengen, men heller returnerer en kopi med de konverterte bokstavene.

## Dypdykk

Det er viktig å merke seg at konvertering til små bokstaver ikke alltid vil gi den ønskede utfall på grunn av forskjeller i tegnsett. For eksempel vil konvertering av en tysk ß (scharfes-s) til små bokstaver resultere i en to-bokstavs-streng (ss) i stedet for den ene riktige bokstaven. Derfor er det viktig å velge en passende funksjon avhengig av konteksten og språket som behandles.

Et annet aspekt å vurdere er ytelse. Konvertering av en streng til små bokstaver involverer en hel del underliggende prosesser og kan påvirke tiden det tar å utføre en oppgave. Det er derfor nyttig å utføre benchmarking for å velge den mest optimale løsningen for ditt spesifikke behov.

## Se også
- [String-modulen i Elixir](https://hexdocs.pm/elixir/String.html#downcase/2)
- [UTF-8 og tegnsett i Elixir](https://hexdocs.pm/elixir/1.12/elixir/Unicode.html)
- [Elixir Tips & Tricks: Case Transformations](https://dev.to/olivierandsm/elixir-tips-tricks-case-transformations-36p5)