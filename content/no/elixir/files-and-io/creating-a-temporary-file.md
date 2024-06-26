---
date: 2024-01-20 17:39:49.687813-07:00
description: "How to: I Unix-lignende systemer, som for eksempel Linux og macOS, har\
  \ midlertidige filer en lang historie for \xE5 hjelpe programmerere med \xE5 h\xE5\
  ndtere\u2026"
lastmod: '2024-04-05T22:50:54.473717-06:00'
model: gpt-4-1106-preview
summary: "I Unix-lignende systemer, som for eksempel Linux og macOS, har midlertidige\
  \ filer en lang historie for \xE5 hjelpe programmerere med \xE5 h\xE5ndtere mellomlagring."
title: Opprette en midlertidig fil
weight: 21
---

## How to:
```elixir
# Bruk File modulen til å opprette en midlertidig fil
{:ok, file_path} = File.mktemp()
IO.puts "Midlertidig fil opprettet: #{file_path}"

# Skriv noe data til den midlertidige filen
File.write!(file_path, "Hei fra Elixir!")

# Les og vis innholdet
IO.puts "Innhold av midlertidig fil:"
IO.puts File.read!(file_path)
```

Forventet resultat:

```
Midlertidig fil opprettet: /tmp/randomfilename123
Innhold av midlertidig fil:
Hei fra Elixir!
```

## Deep Dive
I Unix-lignende systemer, som for eksempel Linux og macOS, har midlertidige filer en lang historie for å hjelpe programmerere med å håndtere mellomlagring. Tradisjonelt ligger de i `/tmp` katalogen. I Elixir land, ivaretar vi tradisjonen ved å utnytte Erlangs sterke filsystemstøtte.

Alternativ til `File.mktemp` kunne være å bruke biblioteker som `Temp` for mer avansert funksjonalitet, eller lage din egen logikk med `System.unique_integer` for å generere unike filnavn.

Elixir håndterer også rydding av midlertidige filer for deg. Normalt sett, håndterer operativsystemet det, men man kan også manuelt slette filer etter bruk med `File.rm`.

## See Also
- Elixir's offisielle dokumentasjon for File-modulen: https://hexdocs.pm/elixir/File.html
- Unix `/tmp` katalogen: https://en.wikipedia.org/wiki/Temporary_folder
- Elixir Forum for diskusjoner og spørsmål: https://elixirforum.com
