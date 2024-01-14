---
title:    "Python: Skriver tester"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/python/writing-tests.md"
---

{{< edit_this_page >}}

## Hvorfor

Når man skriver kode, kan det være fristende å hoppe over testingen og bare fokusere på å få programmet til å kjøre. Men å skrive tester er en essensiell del av en god utviklingsprosess, og kan spare deg for mye tid og frustrasjon på sikt.

## Slik gjør du det

Det finnes flere ulike tester du kan skrive for å forsikre deg om at koden din fungerer som den skal. La oss se på et enkelt eksempel med en funksjon som adderer to tall:

```Python
def adder(a, b):
    return a + b

assert adder(2, 3) == 5
assert adder(5, 5) == 10
```

Her har vi skrevet to assert-statements som sjekker funksjonen vår med ulike input. Hvis testene feiler vil vi få en AssertionError, og vi kan da gå tilbake og rette opp feilen. På denne måten kan vi raskt finne og fikse feil i koden vår før vi publiserer den til produksjon.

Et annet viktig aspekt ved testing er å sørge for at koden vår fungerer med ulike scenarier og input. For eksempel kan vi bruke en innebygd modul i Python som heter unittest, og skrive mer omfattende tester for vår adder-funksjon:

```Python
import unittest

class TestAdder(unittest.TestCase):

    def test_adder(self):
        self.assertEqual(adder(3, 4), 7)
        self.assertEqual(adder(-5, 5), 0)
        self.assertNotEqual(adder(2, 2), 6)
        self.assertRaises(TypeError, adder, 2, 'string')

if __name__ == '__main__':
    unittest.main()
```

Her har vi brukt testmetoder fra unittest-modulen for å sammenligne forventet output med det faktiske outputet fra funksjonen vår. Vi kan også teste for spesifikke feil, som for eksempel når vi gir inn en string som input i stedet for tall. Ved å strukturere testkoden vår på denne måten kan vi enkelt legge til flere tester etterhvert som koden vår blir mer kompleks.

## Dypdykk

Det finnes flere ulike former for testing, som for eksempel enhetstesting, integrasjonstesting og end-to-end testing. Det kan også være lurt å kombinere manuell testing med automatiserte tester for å få et mer helhetlig bilde av koden vår.

En annen viktig del av testing er å sørge for at testene våre er pålitelige og gir oss riktig informasjon. Det kan være lurt å ha en balanse mellom å skrive for mange og for få tester, og å sørge for at testene våre dekker alle mulige utfall av koden vår.

## Se også

- [Pytest documentation](https://docs.pytest.org/)
- [Unit testi