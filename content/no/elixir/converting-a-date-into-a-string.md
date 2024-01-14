---
title:    "Elixir: Konvertere en dato til en streng"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Hvorfor

Konvertering av datoer til strings er en vanlig oppgave når du jobber med programmering, spesielt i webapplikasjoner og databaser. Å kunne gjøre dette effektivt kan bidra til å redusere feil og forbedre brukeropplevelsen når det kommer til å vise datoer i et leservennlig format.

## Hvordan gjøre det

Å konvertere en dato til en string er enkelt med hjelp av Elixir-funksjonene `to_string` og `to_char_list`. La oss si at vi har en dato lagret i variabelen `date`:

```Elixir
date = ~D[2020-09-28]
```

For å konvertere denne datoen til en string i Elixir, kan vi bruke følgende syntaks:

```Elixir
to_string(date)
```

Og her er den resulterende outputen:

```Elixir
"2020-09-28"
```

Hvis vi ønsker å endre formatet på datoen, kan vi bruke funksjonen `to_char_list` i stedet. For eksempel, hvis vi ønsker å vise datoen i formatet "dd/mm/yyyy", kan vi bruke følgende kode:

```Elixir
to_char_list(date, "{0}/{1}/{2}")
```

Dette vil gi følgende output:

```Elixir
["28", "09", "2020"]
```

## Dypdykk

Nå som vi har sett hvordan vi kan konvertere datoer til strings, la oss se på noen dypere detaljer om denne prosessen. Elixir bruker Unicode og UTF-8 som standarde koding for strings, som er en vesentlig forskjell fra andre programmeringsspråk. Dette betyr at Elixir støtter et bredt spekter av språk og tegn fra ulike skriftsystemer.

En annen viktig ting å merke seg er at Elixir behandler datoer som en egen datatype, kalt `~D`, som gjør det enklere å håndtere og manipulere datoer i koden. Dette er spesielt nyttig når det kommer til å utføre operasjoner som å legge til eller trekke fra et bestemt antall dager fra en dato.

## Se også

- [Elixir Datatypes](https://elixir-lang.org/getting-started/basic-types.html)
- [Elixir Date Libraries](https://elixirforum.com/t/elixir-date-time-and-calendar-libraries/20309)
- [Elixir Date Functions](https://hexdocs.pm/elixir/DateTime.html# functions)