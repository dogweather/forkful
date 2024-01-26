---
title:                "Interpolering av en streng"
date:                  2024-01-20T17:51:30.319586-07:00
model:                 gpt-4-1106-preview
simple_title:         "Interpolering av en streng"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
String-interpolering lar deg sette inn variabler direkte inn i strenger. Det gjør koden mer lesbar og skriveren raskere.

## Hvordan gjøre det:
```python
navn = "Ola"
alder = 25
print(f"Hei, jeg heter {navn} og jeg er {alder} år gammel.")
# Output: Hei, jeg heter Ola og jeg er 25 år gammel.

# Med mer komplekse uttrykk:
print(f"{navn} vil være {alder + 5} om fem år.")
# Output: Ola vil være 30 om fem år.
```

## Dypdykk
String-interpolering i Python kom virkelig med version 3.6, gjennom "f-strings". Før det brukte vi `%`-operatoren eller `format()`-funksjonen. I `format()` setter du `{}` i strengen og fyller inn med `format(var)`. I en f-string setter du bare et `f` foran og skriver Python-uttrykk inne i `{}`.

Med `%`:
```python
print("Hei, jeg heter %s og jeg er %d år gammel." % (navn, alder))
```

Med `format()`:
```python
print("Hei, jeg heter {} og jeg er {} år gammel.".format(navn, alder))
```

F-strings er ikke bare kortere, de er også raskere fordi de blir omsatt direkte til kode som lager den endelige strengen, mens `format()` gjør en del ekstra arbeid i bakgrunnen.

## Se Også
- PEP 498, som introduserte f-strings: https://www.python.org/dev/peps/pep-0498/
- Python-dokumentasjon om formaterte streng-litteraler: https://docs.python.org/3/reference/lexical_analysis.html#formatted-string-literals
- Python-dokumentasjon om den gamle `%`-formattingen: https://docs.python.org/3/library/stdtypes.html#old-string-formatting
