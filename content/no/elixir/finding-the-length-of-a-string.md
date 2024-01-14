---
title:                "Elixir: Å finne lengden av en streng"
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvis du har jobbet med Elixir-programmeringsspråket, har du sikkert kommet over et viktig konsept - strenger (strings). Strenger er ganske enkelt en samling av alfanumeriske tegn som er brukt til å representere tekstdata. Men hvorfor er det viktig å finne lengden på en streng? Vel, i mange tilfeller må vi håndtere ulike typer data og å vite lengden på en streng kan være nyttig for å håndtere og manipulere dataene på en effektiv måte.

## Hvordan du finner lengden på en streng

Det er ganske enkelt å finne lengden på en streng i Elixir. Vi kan bruke den innebygde funksjonen `String.length()` til å finne antallet tegn i en streng. La oss se på et eksempel på hvordan dette fungerer:

```
Elixir String.length("Hei, verden!")
```

Dette vil gi oss følgende utgang:

```
12
```

Som du kan se, teller funksjonen antallet tegn i strengen, inkludert mellomrom og spesialtegn.

## Dypdykk

Lengden på en streng kan også påvirkes av hvilket tegnsett som brukes. For eksempel, hvis du arbeider med tegnsettet UTF-8, vil funksjonen `String.length()` telle antallet tegn som brukes i UTF-8-delingen av strengen, og ikke det faktiske antallet tegn som vises i strengen.

Hvis du vil finne den faktiske lengden på en streng med UTF-8-tegn, kan du bruke funksjonen `String.codepoints()`. Denne funksjonen konverterer strengen til en liste av koder for hvert tegn, og vi kan deretter bruke `Enum.count()` til å telle antallet koder og dermed finne den faktiske lengden på strengen.

## Se også

- [Offisiell Elixir dokumentasjon for String](https://hexdocs.pm/elixir/String.html)
- [Antall tegn i en streng i Elixir](https://www.geeksforgeeks.org/number-of-characters-in-a-string-in-elixir/)
- [Manipulering av strenger i Elixir](https://medium.com/@ryo810/blog-%EF%BC%9A-string-%E3%82%B9%E3%83%88%E3%83%AA%E3%83%B3%E3%82%B0%E3%82%92%E4%BD%BF%E3%81%A3%E3%81%9F%E6%96%87%E5%AD%97%E5%88%97%E3%82%92%E6%89%B1%E3%81%86%E6%96%B9%E6%B3%95%E3%81%BE%E3%81%A8%E3%82%81-6be77a935200)