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

## Hvorfor

Det er viktig å sjekke om en mappe eksisterer når man utvikler programvare for å sikre at programmer kjører uten feil og uforutsette problemer dukker opp. Dette gjelder spesielt når man håndterer fil- og mappeoperasjoner.

## Hvordan

```Elixir
defp dir_exists?(path) do
  File.dir?(path)
end

defp handle_dir_exists?(path) do
  case dir_exists?(path) do
    true -> IO.puts "Mappen eksisterer"
    false -> IO.puts "Mappen eksisterer ikke"
  end
end
```

Utdata:

```elixir
handle_dir_exists?("/brukere/john/musikk")
# => Mappen eksisterer
handle_dir_exists?("/brukere/kari/dokumenter")
# => Mappen eksisterer ikke
```

## Dypdykk

I Elixir, og andre programmeringsspråk, brukes `File.dir?/1` -funksjonen for å sjekke om en mappe eksisterer på en bestemt sti. Denne funksjonen returnerer `true` hvis en mappe eksisterer og `false` hvis den ikke gjør det. Det er viktig å merke seg at denne funksjonen også kan returnere `false` hvis det ikke er tilgang til mappen, for eksempel hvis brukeren ikke har riktig tillatelse.

I tillegg til `File.dir?/1` -funksjonen, kan man også bruke `File.exists?/1` for å sjekke om en mappe eller fil eksisterer på en gitt sti. Denne funksjonen vil returnere `true` hvis mappen eller filen eksisterer, uavhengig av tillatelser. Det er viktig å vurdere hvilken funksjon som er mest passende for bruk i spesifikke situasjoner.

## Se også

- [Elixir File](https://hexdocs.pm/elixir/File.html)
- [File.dir?/1](https://hexdocs.pm/elixir/File.html#dir?/1)
- [File.exists?/1](https://hexdocs.pm/elixir/File.html#exists?/1)