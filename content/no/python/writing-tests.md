---
title:                "Python: Skrive tester"
programming_language: "Python"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/writing-tests.md"
---

{{< edit_this_page >}}

## Hvorfor

Å skrive tester er en viktig del av å utvikle programmer i Python. Tester sikrer at koden fungerer som den skal og bidrar til å redusere feil og problemer i programvaren din.

## Hvordan

Det å skrive tester kan virke som en skremmende oppgave, spesielt for nybegynnere i Python. Men det er faktisk ganske enkelt å komme i gang. La oss se på et eksempel:

```
def multiply(x, y):
    return x * y

def test_multiply():
    assert multiply(3, 4) == 12
    assert multiply(2, 5) == 10
    assert multiply(-1, 8) == -8
```

I dette eksempelet har vi en enkel funksjon som multipliserer to tall, og en testfunksjon som sjekker om funksjonen returnerer riktig svar. Vi bruker `assert` for å sjekke om koden fungerer som forventet. Hvis alle testene passerer, vil vi få en "OK" -melding, men hvis en test feiler, vil vi få en feilmelding som indikerer hvilken test som feilet og hva den forventede verdien var.

## Deep Dive

Når vi skriver tester, er det viktig å tenke på alle mulige scenarier og sørge for at koden vår håndterer dem på en riktig måte. Dette innebærer å teste for både korrekte og ugyldige inputverdier, samt kantsituasjoner. Å ha gode tester kan også gjøre det enklere å finne og rette feil i koden, siden vi må forstå hvordan koden fungerer for å kunne skrive effektive tester.

Det finnes flere forskjellige testrammeverk som kan hjelpe deg med å skrive tester i Python, for eksempel `unittest` og `pytest`. Det kan være lurt å undersøke og prøve ut forskjellige rammeverk for å finne det som fungerer best for deg og prosjektet ditt.

## Se også

- [Python unittest-dokumentasjon](https://docs.python.org/3/library/unittest.html)
- [Python pytest-dokumentasjon](https://docs.pytest.org/en/stable/)
- [En komplett guide til testdrevet utvikling i Python](https://medium.com/@andy-shea/an-ultimate-guide-to-test-driven-development-in-python-14fb874e6e74)