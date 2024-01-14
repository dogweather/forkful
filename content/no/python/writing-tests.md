---
title:    "Python: Skrive tester"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Hvorfor
Å skrive tester er en viktig del av å være en effektiv programmerer. Ved å skrive tester, kan du sikre at koden din fungerer som den skal og redusere sjansene for feil og bugs. I tillegg kan det hjelpe deg å forstå koden din bedre og gjøre den mer modulær og vedlikeholdbar.

## Hvordan du gjør det
Å skrive tester i Python er en enkel prosess som kan gjøres ved hjelp av innebygde moduler som `unittest` og `pytest`. La oss si at vi har følgende funksjon som skal legge sammen to tall:

```Python
def add(x, y):
    return x + y
```

For å teste denne funksjonen, kan vi skrive følgende kode:

```Python
import unittest

class TestAddFunction(unittest.TestCase):
    def test_add(self):
        self.assertEqual(add(2, 2), 4)
        self.assertEqual(add(5, 10), 15)
```

Her bruker vi `unittest`-modulen til å opprette en testklasse og en testfunksjon som sjekker om `add`-funksjonen returnerer forventede resultater med forskjellige input. Du kan kjøre denne testen ved å kjøre følgende kommando i terminalen:

`python -m unittest test_file.py`

Dette vil kjøre testen og gi deg en melding om alt gikk bra eller om det var noen feil.

## Dypdykk
Det finnes mange mer avanserte metoder og teknikker når det kommer til å skrive tester. Du kan for eksempel bruke forskjellige assert-metoder for å sjekke for forskjellige typer input og output. I tillegg kan du utføre såkalte "edge case testing" der du tester for uvanlige eller ekstreme situasjoner som kan føre til feil i koden din.

Det er også verdt å nevne at en god test bør være uavhengig av andre tester og samtidig gi deg informasjon om hvor og hvorfor en test eventuelt feiler. Dette kan hjelpe deg med å finne og fikse feil i koden din mer effektivt.

## Se også
- [unittest-dokumentasjon](https://docs.python.org/3/library/unittest.html)
- [pytest-dokumentasjon](https://docs.pytest.org/en/stable/)
- [Writing Effective Tests in Python (engelsk)](https://realpython.com/python-testing/)