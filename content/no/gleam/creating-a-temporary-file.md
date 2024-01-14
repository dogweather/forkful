---
title:                "Gleam: Opprettelse av en midlertidig fil"
simple_title:         "Opprettelse av en midlertidig fil"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Å opprette midlertidige filer kan være en nyttig funksjon i programmering, spesielt når du trenger å lagre midlertidige data eller opprette midlertidige konfigurasjonsfiler. Det kan også være nyttig når du ønsker å teste en del av koden din uten å påvirke eksisterende filer eller data.

## Slik gjør du det

For å opprette en midlertidig fil i Gleam, kan du bruke funksjonen `File.temporary_path()`. Denne funksjonen tar inn en filbane og oppretter en midlertidig fil med tilfeldig navn i den angitte banen. Hvis ingen sti er angitt, vil filen bli opprettet i det midlertidige katalogen til systemet.

```
Gleam import File

midlertidig_fil = File.temporary_path("midlertidig/test.fil")

```

Du kan deretter bruke denne midlertidige filen på samme måte som en hvilken som helst annen fil. For eksempel kan du skrive data til filen ved hjelp av funksjonen `File.write()`, og lese data fra filen ved hjelp av funksjonen `File.read()`.

```
Gleam import File

midlertidig_fil = File.temporary_path("midlertidig/test.fil")
File.write(midlertidig_fil, "Dette er bare en midlertidig fil.")
innhold = File.read(midlertidig_fil)

```

Når koden din er ferdig, må du huske å slette den midlertidige filen ved å bruke funksjonen `File.delete()` for å sørge for at du ikke fyller systemet med ubrukte filer.

## Mer avansert

Det er også mulig å lage midlertidige filer ved hjelp av funksjonen `File.tempfile()`, som tar inn en filbane og en funksjon. Funksjonen vil da bli kjørt, og filen vil bli slettet når funksjonen er ferdig. Dette kan være nyttig hvis du trenger å kjøre en kode som lager en midlertidig fil og garanterer at filen blir slettet etter bruk.

```
Gleam import File

midlertidig_fil = File.tempfile("midlertidig/test.fil", fn ->
  // Kjører koden som lager og bruker en midlertidig fil
  "Dette er en midlertidig fil."
)

```

## Se også

- [Gleam sin offisielle dokumentasjon om filemodulen](https://gleam.run/articles/working-with-files)
- [Gleam sin offisielle dokumentasjon om funksjoner](https://gleam.run/articles/functions)