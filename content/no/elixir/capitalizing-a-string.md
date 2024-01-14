---
title:                "Elixir: Stor bokstaver i en streng"
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Det å kapitalisere en streng, også kjent som å gjøre første bokstav i en streng stor, kan være en viktig del av tekstbehandling. Det kan gjøre teksten din mer lesbar eller være en nødvendig del av en bestemt syntaks. Uansett hvilken grunn du har for å ville kapitalisere en streng, så er det en enkel oppgave å utføre i Elixir.

## Hvordan

For å kapitalisere en streng i Elixir, kan du bruke funksjonen `String.capitalize/1`. Denne funksjonen tar inn en streng som argument og returnerer en ny streng med første bokstav gjort stor.

```Elixir
String.capitalize("hei") # => "Hei"
String.capitalize("GIKK") # => "Gikk"
String.capitalize("eLiXiR") # => "ELiXiR"
```

Som du kan se i eksemplene over, vil funksjonen beholde resten av strengen uendret og kun gjøre første bokstav stor. Dette er nyttig hvis du vil beholde resten av teksten i samme form som den var i, men bare ønsker å gjøre den mer lesbar med en stor bokstav i begynnelsen.

## Dykk Dypere

Hvis du ønsker å kapitalisere flere bokstaver enn bare den første, kan du bruke funksjonen `String.capitalize/2`. Denne funksjonen tar inn et ekstra argument som angir hvor mange bokstaver fra starten av strengen som skal kapitaliseres.

```Elixir
String.capitalize("dette er en streng", 10) # => "Dette er en streng"
String.capitalize("test123", 4) # => "Test123"
```

I de ovennevnte eksemplene vil `String.capitalize/2` kun kapitalisere de første 10 bokstavene i strengen og returnere den nye strengen. Dette kan være nyttig hvis du for eksempel ønsker å kapitalisere kun det første ordet i en setning for å gjøre det mer leselig.

## Se Også

- Offisiell Elixir dokumentasjon for `String.capitalize/1`: https://hexdocs.pm/elixir/String.html#capitalize/1
- Offisiell Elixir dokumentasjon for `String.capitalize/2`: https://hexdocs.pm/elixir/String.html#capitalize/2
- ElixirSchool: https://elixirschool.com/en/lessons/basics/string/#capitalize