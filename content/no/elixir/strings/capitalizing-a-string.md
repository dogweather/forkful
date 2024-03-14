---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:04:47.943399-07:00
description: "\xC5 kapitalisere en streng involverer \xE5 konvertere den f\xF8rste\
  \ bokstaven i strengen til stor bokstav, mens man s\xF8rger for at resten av bokstavene\
  \ er i sm\xE5\u2026"
lastmod: '2024-03-13T22:44:40.427042-06:00'
model: gpt-4-0125-preview
summary: "\xC5 kapitalisere en streng involverer \xE5 konvertere den f\xF8rste bokstaven\
  \ i strengen til stor bokstav, mens man s\xF8rger for at resten av bokstavene er\
  \ i sm\xE5\u2026"
title: Sette stor bokstav i en streng
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å kapitalisere en streng involverer å konvertere den første bokstaven i strengen til stor bokstav, mens man sørger for at resten av bokstavene er i små bokstaver. Denne handlingen er vanligvis nødvendig for formatering av brukerinndata eller visning av tekst i brukergrensesnitt, hvor konsistens og lesbarhet er viktig.

## Hvordan:

Elixir tilbyr en enkel måte å kapitalisere strenger på ved hjelp av sine innebygde funksjoner uten behov for tredjepartsbibliotek. Her er et enkelt eksempel:

```elixir
streng = "elixir programmering"
kapitalisert_streng = String.capitalize(streng)
IO.puts kapitalisert_streng
```

Resultat:

```
Elixir programmering
```

For tilfeller hvor mer kontroll eller kompleks kapitaliseringslogikk er nødvendig, kan du kombinere forskjellige String-funksjoner. For eksempel, hvis du vil kapitalisere hvert ord i en setning, kan du splitte setningen inn i ord, kapitalisere hver enkelt, og deretter sette dem sammen igjen:

```elixir
setning = "elixir er gøy"
kapitalisert_setning = setning 
                        |> String.split() 
                        |> Enum.map(&String.capitalize/1) 
                        |> Enum.join(" ")

IO.puts kapitalisert_setning
```

Resultat:

```
Elixir Er Gøy
```

Selv om Elxirs standardbibliotek dekker de fleste behov, for mer nyansert tekstmanipulering, inkludert avansert strengkapitalisering, kan du utforske tredjepartsbiblioteker slik som Cldr for internasjonalisering, som kan tilby steds-spesifikk kapitaliseringsoppførsel.
