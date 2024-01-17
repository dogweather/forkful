---
title:                "Lage en midlertidig fil"
html_title:           "Elixir: Lage en midlertidig fil"
simple_title:         "Lage en midlertidig fil"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Hva og hvorfor?
Oppretting av midlertidige filer er en vanlig praksis i programmering. Dette innebærer å opprette en midlertidig fil som kun eksisterer i en kort periode under kjøring av programmet. Dette gjøres vanligvis for å lagre midlertidig data eller for å utføre en midlertidig operasjon.

## Slik gjør du det:
I Elixir er det flere måter å opprette en midlertidig fil på. Den enkleste metoden er å bruke funksjonen `Tempfile.open/2`, som tar inn to argumenter: et prefix og et suffiks. Prefixet vil være de første tegnene i filnavnet, mens suffikset vil være de siste tegnene.

```Elixir
faktorius = "KodeKongen"
{:ok, fil} = Tempfile.open(faktorius, ".txt")
IO.puts fil.path
# Ut: "/tmp/KodeKongen20210223-19993-46jrqo.txt"
```

Her kan du se at filen automatisk får et unikt navn basert på prefix og dato og klokkeslett når den blir opprettet. Du kan deretter skrive til filen eller lese fra den, og når du er ferdig kan du enkelt slette den ved å bruke `File.delete/1`.

## Dykk dypere:
Oppretting av midlertidige filer har vært en vanlig praksis i programmering i lang tid. Men i dagens verden hvor data lagres i skyen og kjøretider stadig blir kortere, kan det være bedre å bruke alternative metoder for å lagre midlertidig data. Noen alternativer kan være å bruke en in-memory database eller å utnytte cache-systemer.

I Elixir er også prosesser en kraftig og effektiv måte å jobbe med midlertidig data på. Prosesser er lette og kan raskt opprettes og slettes, noe som gjør dem ideelle for midlertidig data eller operasjoner som bare skal kjøres for en kort periode.

## Se også:
- [Elixir Docs - Tempfile module](https://hexdocs.pm/elixir/Tempfile.html)
- [Elixir Docs - Processes](https://elixir-lang.org/getting-started/processes.html)
- [Elixir School - File IO](https://elixirschool.com/en/lessons/basics/files/)