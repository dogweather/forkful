---
title:                "Konvertere en streng til små bokstaver"
html_title:           "Python: Konvertere en streng til små bokstaver"
simple_title:         "Konvertere en streng til små bokstaver"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hvorfor

Å konvertere en streng til små bokstaver kan være nyttig når du jobber med tekstdokumenter eller programmering. Dette kan gjøre det enklere å sammenligne tekster og finne bestemte ord eller uttrykk.

## Hvordan

For å konvertere en streng til små bokstaver i Python, kan du bruke funksjonen "lower()". Her er et eksempel på hvordan du kan bruke denne funksjonen:

```Python
tekst = "HELLO WORLD"
print(tekst.lower())
```
Dette vil resultere i utskriften "hello world". Som du kan se, har alle bokstavene blitt konvertert til små bokstaver.

Du kan også bruke "lower()" funksjonen på en variabel som allerede inneholder en streng. For eksempel:

```Python
navn = "MARTHA"
print("Hei", navn.lower())
```
Dette vil skrive ut "Hei martha". Som du kan se, kan du også kombinere funksjonen med andre Python-kommandoer for å lage mer avansert kode.

## Dypdykk

Det finnes flere forskjellige måter å konvertere en streng til små bokstaver på i Python, men "lower()" funksjonen er en av de enkleste og mest effektive. Det er også viktig å huske at denne funksjonen bare gjelder for bokstaver, og vil ikke påvirke tall eller symboler i strengen. Du kan også bruke "upper()" funksjonen for å konvertere en streng til store bokstaver.

## Se også

- [Python dokumentasjon for "lower()" funksjonen](https://docs.python.org/3/library/stdtypes.html#str.lower)
- [Tutorial om strings i Python (på norsk)](https://realpython.com/python-strings/)