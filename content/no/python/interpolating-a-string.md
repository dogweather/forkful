---
title:                "Interpolering av en streng"
html_title:           "Python: Interpolering av en streng"
simple_title:         "Interpolering av en streng"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Når du jobber med strenger (tekst), kan du møte på tilfeller der du trenger å sette inn variabler eller verdier i en streng. Dette kalles å interpolere en streng, og det er en viktig del av programmering for å gjøre teksten mer dynamisk og variabel.

## Slik gjør du:

Du kan interpolere en streng ved å bruke f-strings i Python. Dette gjøres ved å skrive f foran strengen og deretter plassere variabelnavnet eller uttrykket du ønsker å sette inn i strengen inne i krøllparanteser. Se eksempelet under:

```python
navn = "Marie"
alder = 25
print(f"Hei, mitt navn er {navn} og jeg er {alder} år gammel.")
```

Dette vil gi følgende output:

```python
Hei, mitt navn er Marie og jeg er 25 år gammel.
```

Du kan også bruke formateringsmetoden `.format()` for å interpolere en streng. Dette gjøres ved å plassere variabelnavnet eller uttrykket du ønsker å sette inn i strengen inne i krøllparanteser, men uten f foran strengen. Se eksempelet under:

```python
navn = "Maria"
print("Hei, mitt navn er {}.".format(navn))
```

Dette vil gi følgende output:

```python
Hei, mitt navn er Maria.
```

## Dypdykk:

Interpolasjon av strenger har vært en del av programmering helt siden programmeringsspråket C ble utviklet på 1970-tallet. Før dette var det vanlig å bare skrive variabelnavnet eller uttrykket inne i en streng, noe som gjorde teksten mindre fleksibel. Det finnes også andre måter å interpolere strenger på, som for eksempel ved å bruke `str.format()` eller %-formatering i Python. Uansett hvilken metode du velger, er det viktig å bruke interpolasjon for å gjøre teksten mer dynamisk og effektiv.

## Se også:

- [Python dokumentasjon om f-strings](https://docs.python.org/3/tutorial/inputoutput.html#formatted-string-literals)
- [Python dokumentasjon om `.format()`](https://docs.python.org/3/library/string.html#format-strings)
- [Artikkel om %-formatering i Python](https://realpython.com/python-formatted-output/)