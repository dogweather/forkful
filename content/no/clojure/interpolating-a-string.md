---
title:                "Interpolering av en streng"
html_title:           "Bash: Interpolering av en streng"
simple_title:         "Interpolering av en streng"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Interpolering av en tekststreng er prosessen med å sette inn og erstatte variable og uttrykk i en streng med deres evaluerte verdier. Dette gjør det enklere for programmerere å generere dynamiske strenger og forbedrer lesbarheten av koden.

## Slik gjør du:
I Clojure bruker vi `format` funksjon for å interpolere en tekststreng. Her er et eksempel:

```Clojure
(let [name "Ola" age 25]
  (format "Hei, jeg er %s og jeg er %d år gammel." name age))
```

Dette vil skrive ut:

```
"Hei, jeg er Ola og jeg er 25 år gammel."
```

## Dypdykk
Interpolering av tekststrenger har røtter tilbake til tidlig programmering, og er felles for mange programmeringsspråk. 

Alternative metoder kan inkludere sammenkobling av strenger, men det kan ofte være mindre effektivt og vanskeligere å lese.

En nøkkel detalj med Clojures `format`-funksjon er at den bruker Java's `String.format` under panseret, noe som betyr at den støtter de samme formateringsmetodene og -syntaxene som Java.

## Se også
For mer detaljert informasjon om tekststrenginterpolering og `format` funksjonen, sjekk ut:

- Clojure's offisielle dokumentasjon for `format`-funksjonen: https://clojuredocs.org/clojure.core/format
- En guide til Java's `String.format`: https://dzone.com/articles/java-string-format-examples
- En mer omfattende artikkel om tekststrenginterpolering i forskjellige språk: https://www.digitalocean.com/community/tutorials/how-to-use-string-interpolation-in-modern-programming-languages-a-comparison