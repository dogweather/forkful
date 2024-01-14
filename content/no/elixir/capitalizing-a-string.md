---
title:    "Elixir: Konvertere en streng til stor bokstav"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/elixir/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

I Elixir-programmering er det ofte nødvendig å manipulere strenger, enten det er for å formatere utdata eller behandle input fra en bruker. En vanlig operasjon som man ofte støter på, er å gjøre den første bokstaven i en streng stor. Denne enkle handlingen kan hjelpe med å gjøre koden mer lesbar og ryddig. I denne bloggposten skal vi se på hvordan man kan gjøre dette i Elixir.

## Hvordan

For å gjøre den første bokstaven i en streng stor, kan man bruke funksjonen `String.capitalize/1` som tar inn en streng som argument. La oss se på et eksempel:

```Elixir
iex> String.capitalize("heisann")
"Heisann"
```

Som du kan se, blir den første bokstaven i strengen "heisann" gjort stor. Dette gjelder også for strenger som allerede er formater med store bokstaver:

```Elixir
iex> String.capitalize("HEISANN")
"Heisann"
```

Det er verdt å merke seg at denne funksjonen bare endrer den første bokstaven i en streng, og ikke resten av strengen.

## Deep Dive

Dette høres kanskje veldig enkelt ut, men det er faktisk mer komplekst bak kulissene. Funksjonen `String.capitalize/1` tar i bruk Unicode-karakterer og støtter derfor også internasjonale språk. Det betyr at den også kan behandle tegn som aksenter og andre diakritiske tegn på riktig måte.

Det er også verdt å merke seg at denne funksjonen ikke endrer originale strengen, men returnerer en ny streng med den samme verdien, men med den første bokstaven gjort stor. Dette er i tråd med Elixir sin filosofi om å være en funksjonell programmeringsspråk.

## Se Også

- [Elixir Docs for String.capitalize/1](https://hexdocs.pm/elixir/String.html#capitalize/1)
- [Elixir School - Strings](https://elixirschool.com/en/lessons/basics/basics/#strings)
- [Elixir Forum - Capitalize First Letter in String](https://elixirforum.com/t/capitalize-first-letter-in-string/1014)