---
title:                "Gleam: Søking og erstatning av tekst"
programming_language: "Gleam"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Hvorfor

Vi har alle vært der - å skrive store stykker med kode bare for å innse at vi må endre en enkelt tekststreng. Enten det er en stavefeil eller en endring i programvaren, kan det være en tidkrevende oppgave å manuelt bytte ut alle forekomster av tekststrengen. Heldigvis gjør Gleam det enkelt å søke og erstatte tekst i koden din, slik at du kan spare tid og unngå unødvendig frustrasjon.

## Slik gjør du det

Det er to hovedfunksjoner i Gleam som lar deg søke og erstatte tekst: `gsub` og `gsub_all`. Begge funksjonene tar inn en tekststreng du ønsker å bytte ut, og en ny tekststreng som skal erstatte den gamle.

La oss si at du ønsker å bytte ut alle forekomster av ordet "katt" med ordet "hund" i følgende tekststreng:

```
Gleam, språket som får deg til å føle at du kan kode som en katt.
```

Du kan bruke `gsub` for å bytte ut den første forekomsten av "katt":

```Gleam
gsub("katt", "hund", "Gleam, språket som får deg til å føle at du kan kode som en katt.")
```

Outputen blir: `Gleam, språket som får deg til å føle at du kan kode som en hund.`

Hvis du ønsker å bytte ut alle forekomster av "katt", kan du bruke `gsub_all`:

```Gleam
gsub_all("katt", "hund", "Gleam, språket som får deg til å føle at du kan kode som en katt.")
```

Outputen blir: `Gleam, språket som får deg til å føle at du kan kode som en hund.`

En annen nyttig funksjon er `gsub_all_cases`, som gjør det samme som `gsub_all`, men tar inn et `"ignore_ces"` argument som ignorerer forskjeller i store og små bokstaver. Dette betyr at både "katt" og "KATT" vil bli byttet ut med "hund".

## Dypdykk

Under panseret bruker Gleam NIF-er for å kalle på funksjoner i Erlangs `:string`-modul, som håndterer tekstmanipulering. Dette øker hastigheten på `gsub` og `gsub_all` sammenlignet med å skrive tilsvarende funksjoner i ren Gleam-kode.

En annen interessant detalj er at Gleam har støtte for regexp (regular expressions) ved hjelp av `Regex`-modulen. Dette gir større fleksibilitet når du skal søke og erstatte tekst.

## Se også

- Offisiell Gleam-nettside: https://gleam.run/
- Dokumentasjon om tekstmanipulering i Gleam: https://gleam.run/manual/strings.html
- Gleam på GitHub: https://github.com/gleam-lang/gleam