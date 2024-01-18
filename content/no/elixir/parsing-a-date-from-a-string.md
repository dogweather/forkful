---
title:                "Analysering av dato fra en streng"
html_title:           "Elixir: Analysering av dato fra en streng"
simple_title:         "Analysering av dato fra en streng"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

# Hva & Hvorfor?

Å parse en dato fra en streng er en måte for dataprogrammer å konvertere en tekstrepresentasjon av en dato til et datotyper som kan brukes av programmet. Dette kan være nyttig for å sortere, filtrere eller beregne basert på datoer. Programmere gjør dette for å få mer nøyaktig og effektiv behandling av datoer i sine programmer.

# Hvordan:

Å parse en dato fra en streng er enkelt med Elixir. Bare bruk ```Date.from_iso8601("datostreng")``` funksjonen, hvor "datostreng" er i ISO 8601 format, som for eksempel "2021-10-02". Dette vil returnere en ```{:ok, %Date{}}``` tuple med den parsede datoen som kan brukes i programmet ditt.

```Elixir
datostreng = "2021-10-02"
{:ok, dato} = Date.from_iso8601(datostreng)

IO.puts "Datoen er #{Date.to_string(dato)}"

#Output: Datoen er 2021-10-02
```

# Dypdykk:

Å parse datoer fra strenger har vært en viktig del av dataprogrammering i lang tid. Før ISO 8601 standarden var det ingen standardisert måte å representere datoer på, noe som gjorde det vanskelig å håndtere forskjellige datoformater. I dag finnes det også alternativer for å parse datoer fra strenger i Elixir, som ```Timex``` biblioteket.

Når du bruker ```Date.from_iso8601``` funksjonen, vil Elixir automatisk konvertere datoen til UTC-tidssone. Hvis du ønsker å beholde den opprinnelige tidszonen, kan du bruke ```Date.from_iso8601!(datostreng, :local)``` funksjonen.

# Se også:

- [ISO 8601 Standard](https://www.iso.org/iso-8601-date-and-time-format.html)
- [Timex biblioteket](https://github.com/bitwalker/timex)