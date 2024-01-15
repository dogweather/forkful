---
title:                "Å finne lengden av en streng"
html_title:           "Bash: Å finne lengden av en streng"
simple_title:         "Å finne lengden av en streng"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Har du noen gang lurt på hvor mange tegn en tekststreng inneholder? Kanskje du jobber med dataanalyse eller programmering, og trenger å vite lengden på en streng for å kunne behandle den riktig. Uansett årsak, å kunne finne lengden på en streng er en nyttig ferdighet å ha i verktøykassen din.

## Hvordan

Det finnes flere måter å finne lengden på en streng i Bash på. En av de vanligste metodene er å bruke kommandoen `expr length`, som tar inn en streng og returnerer antall tegn i den. La oss se på et eksempel:

```Bash
streng="Hei, verden!"
echo `expr length $streng`
```

Dette vil returnere verdien 12, siden tekststrengen består av 12 tegn. Merk at vi først må sette strengen i en variabel (`streng=`) og så bruke variabelen i kommandoen med `$`. Ellers vil kommandoen ikke vite hvilken streng den skal telle antall tegn i.

En annen metode er å bruke parameterutvidelsen `${#string}`, som også returnerer lengden på en streng. Her er et eksempel:

```Bash
streng="Hei, verden!"
echo ${#streng}
```

Dette vil også gi oss verdien 12 som output. Forskjellen her er at vi ikke trenger å bruke `expr` kommandoen, bare parameterutvidelsen `${#}`.

## Dypdykk

Visste du at det også er mulig å telle antall tegn i en fil ved hjelp av Bash? For å gjøre dette, kan du bruke kommandoen `wc`, som står for "word count". Siden denne kommandoen er ment for å telle ord, må du bruke parameterutvidelsen `${#}` for å bare telle tegn. Her er et eksempel:

```Bash
fil="eksempel.txt"
tegn=$(wc -m < $fil)
echo $tegn
```

I dette tilfellet vil variabelen `tegn` inneholde antall tegn i filen `eksempel.txt` og output vil være det samme tallet. Igjen, merk at vi må bruke parameterutvidelsen for å bare få antall tegn.

## Se også

- [Bash parameterutvidelser](https://wiki.bash-hackers.org/syntax/pe)
- [Kommandoen `wc`](https://www.hostinger.com/tutorials/linux-wc-command)