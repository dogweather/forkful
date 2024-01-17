---
title:                "Stor bokstav på en streng"
html_title:           "Elixir: Stor bokstav på en streng"
simple_title:         "Stor bokstav på en streng"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hva & hvorfor?

Å sette bokstaver i store bokstaver, også kjent som "kapitalisering", er en vanlig oppgave i programmering. Dette innebærer å gjøre alle bokstaver i en string eller tekstblokk store. Mange programmerere gjør dette for å gjøre koden lettere å lese og forstå, spesielt når variabelnavn eller funksjonsnavn er lange og komplekse.

## Hvordan:

```Elixir
string = "dette er en test string"
String.upcase(string)
```

Output:

```Elixir
"DETTE ER EN TEST STRING"
```

I dette eksemplet viser vi hvordan du kan bruke funksjonen `upcase` fra `String` modulen i Elixir for å kapitalisere en string. Vi først definerer en variabel `string` med en vanlig tekststreng, og deretter bruker vi `String.upcase` funksjonen for å gjøre alle bokstavene i stringen til store bokstaver. Output viser dette resultatet.

## Dykk dypere:

Historisk sett har kapitalisering vært viktig for å skille mellom små og store bokstaver i kodingspråk som er case-sensitive. Dette betyr at programmeringsspråk som skiller mellom små og store bokstaver, vil behandle "A" og "a" som to forskjellige bokstaver. Alternativt kan noen programmerere også bruke understrykningstegn "_" for å indikere separate ord i variabelnavn istedenfor å bruke store bokstaver.

I tillegg til `String.upcase` funksjonen i Elixir, finnes det også andre måter å kapitalisere en string på. Du kan for eksempel bruke `String.capitalize` for å kun gjøre første bokstav i stringen til en stor bokstav, eller `String.capitalize_words` for å gjøre første bokstav i hvert ord i en string til en stor bokstav.

## Se også:

- Elixir `String` modul dokumentasjon: https://hexdocs.pm/elixir/String.html
- Elixir `String.capitalize` funksjonsdokumentasjon: https://hexdocs.pm/elixir/String.html#capitalize/1
- Elixir `String.capitalize_words` funksjonsdokumentasjon: https://hexdocs.pm/elixir/String.html#capitalize_words/1