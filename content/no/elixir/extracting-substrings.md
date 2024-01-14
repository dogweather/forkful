---
title:                "Elixir: Uttrekking av delstrenger"
simple_title:         "Uttrekking av delstrenger"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/extracting-substrings.md"
---

{{< edit_this_page >}}

# Hvorfor

Hvorfor skulle man ønske å trekke ut substringer i Elixir? Det kan være flere grunner til dette, men det mest vanlige er å håndtere tekst- og dataanalyse. Substrings er deler av en streng som består av en sekvens av tegn, og ved å ekstrahere disse kan man få tilgang til spesifikke deler av en tekst.

# Hvordan

For å trekke ut substringer i Elixir, kan vi bruke funksjonen `String.slice/3`, som tar tre argumenter: strengen vi ønsker å trekke ut fra, startindeksen og slutindeksen til den ønskede substringsen. La oss se på et eksempel for å illustrere dette:

```Elixir
str = "Elixir er et fantastisk programmeringsspråk!"

# Eksempel 1: Trekke ut første del av strengen
String.slice(str, 0, 6)

# Output: "Elixir"

# Eksempel 2: Trekke ut en del av strengen ved å bruke negative indekser
String.slice(str, -14, -8)

# Output: "språk!"
```

Vi kan også bruke funksjonen `String.split/3` for å dele en streng basert på et visst separator-tegn og deretter få tilgang til en bestemt substring. La oss se på et annet eksempel:

```Elixir
str = "Elixir er et fantastisk programmeringsspråk!"

# Eksempel 3: Dele strengen på mellomrom og trekke ut den siste delen
[String.split(str, " ") |> List.last]

# Output: "programmeringsspråk!"
```

# Dypdykk

Nå som vi har lært hvordan vi kan trekke ut substringer, la oss ta en titt på noen dypere konsepter. En viktig ting å merke seg er at indeksering i Elixir starter fra 0, noe som betyr at den første delen av en streng har indeks 0, den andre har indeks 1, og så videre.

I tillegg er det viktig å være klar over at Elixir har unicode-støtte, så det er viktig å ta hensyn til dette når man arbeider med tekst og trekk ut substringer.

# Se også

- [Elixir dokumentasjon om strenger](https://hexdocs.pm/elixir/String.html)
- [Unicode i Elixir](https://elixir-lang.org/getting-started/unicode-and-strings.html)