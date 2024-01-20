---
title:                "Konvertere en streng til små bokstaver"
html_title:           "Arduino: Konvertere en streng til små bokstaver"
simple_title:         "Konvertere en streng til små bokstaver"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Bash programmering: Konverter strenger til små bokstaver

## Hva & Hvorfor?
Å konvertere en streng til små bokstaver betyr å endre alle store bokstaver i strengen til deres tilsvarende små bokstaver. Dette er nyttig når vi vil normalisere data, for eksempel for å gjøre en case-insensitiv sammenligning.

## Hvordan:
Å konvertere en streng til små bokstaver i Bash er så enkelt som å bruke `tr` kommandoen:

```Bash
tekst="Jeg Liker Å Kode"
echo $tekst | tr '[:upper:]' '[:lower:]'
```
Produksjon:
```Bash
jeg liker å kode
```

## Dypdykk
`tr` kommandoen i Bash har en lang historie som strekker seg tilbake til Unix-operativsystemene på 1970-tallet. Den ble utviklet som et verktøy for å translitere eller slette karakterer.

Et alternativ til `tr` er `awk` kommandoen:
```Bash
tekst="Jeg Liker Å Kode"
echo $tekst | awk '{print tolower($0)}'
```

Utførelsen av å konvertere en streng til små bokstaver avhenger av det underliggende operativsystemet og dets standard C-bibliotek. `tr` bruker C-bibliotekets tolower() -funksjon for å konvertere individuelle tegn til små bokstaver.

## Se også:
- `tr` man side: [Link](http://man7.org/linux/man-pages/man1/tr.1.html)
- `awk` man side: [Link](http://man7.org/linux/man-pages/man1/awk.1p.html)
- GNU C Library dokumentasjon på tolower(): [Link](https://www.gnu.org/software/libc/manual/html_node/Case-Conversion.html)