---
title:                "Interpolering av en streng"
html_title:           "Bash: Interpolering av en streng"
simple_title:         "Interpolering av en streng"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Stringinterpolasjon er teknikken å sette, eller bytte inn, verdier inni en streng. Programmerere bruker det for å lage lesebare koder og formatere strenger mer effektivt.

## Slik gjør du:

I Haskell bruker vi `printf` funksjonen for å interpolere strenger. Her er et enkelt eksempel:

```Haskell
import Text.Printf (printf)

navn = "Ola"
jobb = "Programmør"
printf "Hei, mitt navn er %s og jeg er en %s." navn jobb
```

Resultatet vil være:

```Haskell
"Hei, mitt navn er Ola og jeg er en Programmør."
```
## Dypdykk

Strenginterpolasjon har vært en del av programmeringsspråk siden tidlig i historien. I noen språk, som Perl og Ruby, er det mer innebygd i språket selv. I Haskell er det litt mer begrenset, men `printf` funksjonen gir oss likevel en god del fleksibilitet.

Alternativt kan du bruke biblioteket `interpolate`, som gir et mer "Haskellete" måte å gjøre strenginterpolasjon på.

Implementeringen av strenginterpolasjon i Haskell er avhengig av hvordan `printf` eller `interpolate` er implementert. Det er visitig å nevne at i Haskell handler det meste om funksjonalitet og mindre om lavnivåimplementeringer.

## Se også

For mer informasjon, se:

1. `Printf` https://hackage.haskell.org/package/base-4.14.1.0/docs/Text-Printf.html
2. `Interpolate` https://hackage.haskell.org/package/interpolate