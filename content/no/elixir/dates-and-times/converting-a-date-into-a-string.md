---
date: 2024-01-20 17:36:09.563584-07:00
description: "How to: Elixir h\xE5ndterer datoer med en innebygd modul kalt `DateTime`.\
  \ For \xE5 konvertere en dato til en streng, bruker vi `DateTime.to_string/1`."
lastmod: '2024-03-13T22:44:40.454967-06:00'
model: gpt-4-1106-preview
summary: "Elixir h\xE5ndterer datoer med en innebygd modul kalt `DateTime`."
title: Konvertere en dato til en streng
weight: 28
---

## How to:
Elixir håndterer datoer med en innebygd modul kalt `DateTime`. For å konvertere en dato til en streng, bruker vi `DateTime.to_string/1`:

```elixir
date = ~N[2023-09-17 14:30:00]
string_date = DateTime.to_string(date)
IO.puts(string_date)
```

Output:
```
"2023-09-17 14:30:00"
```

Du kan også bruke `Date.to_string/1` hvis du kun trenger datodelen:

```elixir
date = ~D[2023-09-17]
string_date = Date.to_string(date)
IO.puts(string_date)
```

Output:
```
"2023-09-17"
```

## Deep Dive
I historisk perspektiv har datoformat konverteringer vært viktig for å standardisere kommunikasjon mellom systemer, spesielt før standardiseringen av ISO 8601. Elixir bruker ISO 8601 som standard format for å representere dato og tid som tekst. Dette formatet er klart, entydig og lett å sortere.

Alternativer for konvertering inkluderer bruk av `Timex` biblioteket, som tilbyr mer fleksibilitet med formatering og tidssoner. `Cldr` er et annet bibliotek som støtter multikulturell datumformatering.

Når det gjelder implementeringsdetaljer, benytter `DateTime.to_string/1` seg av kalendermodulen som standard kalender, men det er mulig å tilpasse med andre kalendertyper som støttes av Elixir.

## See Also
For videre lesning, sjekk ut følgende ressurser:
- [Elixir's DateTime documentation](https://hexdocs.pm/elixir/DateTime.html)
- [ISO 8601 Standard](https://www.iso.org/iso-8601-date-and-time-format.html)
- [Timex GitHub repository](https://github.com/bitwalker/timex)
- [Cldr GitHub repository](https://github.com/elixir-cldr/cldr)
