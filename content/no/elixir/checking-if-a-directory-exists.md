---
title:                "Sjekke om en mappe eksisterer"
html_title:           "Elixir: Sjekke om en mappe eksisterer"
simple_title:         "Sjekke om en mappe eksisterer"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Hva og hvorfor?
Sjekke om en mappe eksisterer er en måte for utviklere å kontrollere om en spesifikk mappe finnes i et filsystem. Dette er nyttig for å sjekke om en mappe er tilgjengelig før man prøver å utføre noen operasjoner på den.

## Hvordan:
For å sjekke om en mappe eksisterer i Elixir, kan man bruke funksjonen `File.exists?` og gi den navnet på mappen som et argument. Denne funksjonen returnerer enten `true` eller `false` avhengig av om mappen finnes eller ikke. Her er et eksempel:

```Elixir
if File.exists?("min_mappe") do
  IO.puts "Mappen finnes"
else
  IO.puts "Mappen finnes ikke"
end
```
Dette vil skrive ut enten "Mappen finnes" eller "Mappen finnes ikke" basert på resultatet av sjekken.

## Dypdykk:
Å sjekke om en mappe eksisterer er en vanlig operasjon i mange programmeringsspråk og er nyttig for å sikre at man ikke prøver å utføre operasjoner på en mappe som ikke finnes. Alternativet til å bruke `File.exists?`-funksjonen i Elixir er å bruke `File.read_dir` og sjekke om mappenavnet finnes i resultatene. Men dette vil føre til ekstra kompleksitet og kan være mindre effektivt.

Det er verdt å merke seg at å sjekke om en mappe eksisterer ikke garanterer at den vil være tilgjengelig når man faktisk prøver å utføre en operasjon på den. Dette kan skyldes at mappen kan bli slettet av en annen prosess eller bruker mellom sjekken og operasjonen.

## Se også:
[File-modulen i Elixir](https://hexdocs.pm/elixir/File.html)