---
title:                "Søking og erstatt av tekst"
html_title:           "Rust: Søking og erstatt av tekst"
simple_title:         "Søking og erstatt av tekst"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Hvorfor

Har du noen gang sittet i timesvis og manuelt endret tekst i en fil? Det kan være både kjedelig og tidkrevende, spesielt hvis du gjør det ofte. Søk og erstatt-funksjonaliteten i Rust kan gjøre dette arbeidet mye raskere og enklere for deg!

## Slik gjør du det

Søk og erstatt-funksjonaliteten i Rust gjør det mulig å automatisk søke etter et bestemt mønster i en tekst og erstatte det med et annet mønster. La oss ta en enkel tekstfil som et eksempel: 

```
Dette er en testfil med noen ganger ord som "rust" og noen ganger ord som "Rust".
```

Hvis vi ønsker å endre alle forekomster av "rust" til "Rust" i denne filen, kan vi bruke Rusts `replace` funksjon. Vi definerer først en variabel med den opprinnelige teksten:

```
let tekst = "Dette er en testfil med noen ganger ord som \"rust\" og noen ganger ord som \"Rust\".";
```

Deretter kan vi bruke `replace` sammen med tekstvariabelen og de to mønstrene som skal byttes ut:

```
let ny_tekst = tekst.replace("rust", "Rust");
```

Det blir da opprettet en ny variabel `ny_tekst` med den endrede teksten. Vi kan skrive ut den nye teksten og se at alle forekomster av "rust" er blitt erstattet med "Rust":

```
println!("{}", ny_tekst);
```

Output:
```
Dette er en testfil med noen ganger ord som "Rust" og noen ganger ord som "Rust".
```

## Dypdykk

Å søke og erstatte tekst er en essensiell funksjonalitet for å håndtere og manipulere data. I Rust kan du bruke `replace` funksjonen på både `String` og `&str` (en referanse til en tekst), og du kan også bruke den i kombinasjon med regulære uttrykk. Dette gir stor fleksibilitet og mulighet for mer avanserte søk og erstatningsoperasjoner.

## Se også

- [Rusts dokumentasjon for `replace` funksjonen](https://doc.rust-lang.org/std/string/struct.String.html#method.replace)
- [En tutorial om søking og erstatning i Rust](https://www.tutorialspoint.com/rust/rust_string_replace.htm)
- [En bloggpost om søk og erstatning i Rust](https://blog.knoldus.com/rust-search-and-replace-in-a-file/)