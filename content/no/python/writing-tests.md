---
title:                "Skrive tester"
html_title:           "Python: Skrive tester"
simple_title:         "Skrive tester"
programming_language: "Python"
category:             "Python"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/writing-tests.md"
---

{{< edit_this_page >}}

## Hvorfor

Å skrive tester er en nødvendig del av å skrive god og pålitelig kode. Ved å skrive tester kan du sikre at koden din fungerer som forventet, og det gjør det også enklere å finne og fikse feil i koden.

## Hvordan

For å skrive tester i Python, trenger du et testrammeverk som for eksempel `unittest` eller `pytest`. La oss se på et enkelt eksempel på hvordan du kan skrive og kjøre en test ved hjelp av `unittest`:

```Python
import unittest

# Definer en klasse for testene dine
class TestKalkulator(unittest.TestCase):
    
    # Lag en testfunksjon som starter med "test_"
    def test_addisjon(self):
        # Definer input og forventet output
        x = 5
        y = 10
        forventet_output = 15
        
        # Kjør funksjonen du vil teste
        faktisk_output = addisjon(x, y)
        
        # Sjekk om faktisk output er lik forventet output
        self.assertEqual(faktisk_output, forventet_output)
        
# Kjør testene ved å kjøre denne filen
if __name__ == '__main__':
    unittest.main()
```

Når du kjører denne filen, vil du se resultatet av testen din i terminalen:

```
.
----------------------------------------------------------------------
Ran 1 test in 0.000s
OK
```

Dette betyr at testen din bestod, og funksjonen `addisjon` fungerer som forventet.

## Dypdykk

Når du skriver tester, er det viktig å sørge for at du tester alle mulige tilfeller, også grensetilfeller og ugyldige input. Du kan også bruke `assert`-setninger for å sjekke at verdier er `True` eller `False`, eller at de er av riktig type. Dette vil hjelpe deg med å fange opp eventuelle feil i koden din.

Husk også at tester bør skrives før koden din, slik at du kan følge en "test-drevet utvikling" (TDD) tilnærming. Dette betyr at du skriver tester først, og deretter skriver du koden for å få testene til å passere.

## Se også

- [Dokumentasjon for Pythons `unittest`-rammeverk](https://docs.python.org/3/library/unittest.html)
- [Dokumentasjon for `pytest`-rammeverket](https://docs.pytest.org/en/latest/)