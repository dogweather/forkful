---
title:                "Ekstrahering av substringer"
html_title:           "Fish Shell: Ekstrahering av substringer"
simple_title:         "Ekstrahering av substringer"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Hva og Hvorfor?

Ekstrahering av substringer er en måte å isolere deler av en tekst på basert på bestemte karakterer eller posisjoner. Dette kan være nyttig for programmerere for å manipulere og behandle data på en mer effektiv måte.

## Hvordan?

```Fish Shell``` tilbyr flere metoder for å ekstrahere substringer fra en gitt tekst. En vanlig metode er å bruke "cut" kommandoen, som kan brukes til å velge et gitt antall tegn fra starten eller slutten av en tekstlinje.

```
$ echo "Hei alle sammen" | cut -c 4-8
alle 
```

I dette tilfellet vil "cut" kommandoen returnere tegnene fra posisjon 4 til 8 i den gitt tekstlinjen.

Et annet alternativ er å bruke "sed" kommandoen, som kan brukes til å bytte ut eller erstatte deler av en tekstlinje basert på et gitt mønster.

```
$ echo "Goodbye, World!" | sed 's/Goodbye/Hello/'
Hello, World!
```

## Dykk dypere

Eksistrering av substringer har vært en del av programmering siden de tidlige dagene av UNIX-operativsystemet. I tillegg til "cut" og "sed" finnes det flere verktøy som kan oppnå samme resultat, som for eksempel "grep" og "awk".

Implementeringen av disse verktøyene kan variere basert på hvilket operativsystem de brukes på, men essensen av å ekstrahere substringer forblir den samme.

## Se også

- [Fish Shell dokumentasjon](https://fishshell.com/docs/current/index.html)
- [Kort introduksjon til regulære uttrykk](https://www.regular-expressions.info/intro.html)