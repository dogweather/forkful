---
title:                "Å søke og erstatte tekst"
html_title:           "Gleam: Å søke og erstatte tekst"
simple_title:         "Å søke og erstatte tekst"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Søking og erstatting av tekst er en vanlig oppgave for programvareutviklere. Dette innebærer å finne et bestemt mønster eller tekststreng i en tekst, og erstatte den med en annen tekststreng. Dette kan være nyttig for å håndtere korrekturlesing, oversettelse eller oppdatering av kode.

## Hvordan å:

For å søke og erstatte tekst i Gleam, kan du bruke funksjonen `replace_all`. Her er et eksempel på hvordan du bruker den:

```Gleam
replace_all("Hello world!", "world", "universe")
// output: "Hello universe!"
```

Du kan også bruke regulære uttrykk med `replace_all` for å finne og erstatte et bestemt mønster i en tekst:

```Gleam
replace_all("1 + 2 = 3", "\d+", "10")
// output: "10 + 10 = 10"
```

## Dykk dypere

Søking og erstatting av tekst er en utbredt funksjon i mange programmeringsspråk. I eldre språk som C og Java, må du bruke kompliserte funksjoner for å utføre denne oppgaven, mens det i moderne språk som Gleam er enklere takket være innebygd støtte.

Noen alternative tilnærminger til søking og erstatting inkluderer å bruke eksterne biblioteker, som Regex i Rust, eller å bruke skripting-språk som Python og Perl for å håndtere dette aspektet av programmet ditt.

## Se også

- [Gleam Docs: Strings](https://gleam.run/articles/strings)
- [Regular Expressions in Rust with Regex](https://rust-lang-nursery.github.io/regex/book/index.html)
- [Python Søk og Erstatt](https://docs.python.org/3/library/stdtypes.html#str.replace)