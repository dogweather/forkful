---
title:                "Python: Å skrive tester"
simple_title:         "Å skrive tester"
programming_language: "Python"
category:             "Python"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/writing-tests.md"
---

{{< edit_this_page >}}

## Hvorfor

Når vi skriver kode, er det viktig å sikre at den fungerer som den skal og at den ikke bryter når vi legger til nye funksjoner eller gjør endringer. For å være trygg på at koden vår fungerer, er det essensielt å skrive tester. Tester lar oss kjøre gjennom scenarier og sjekke om koden vår faktisk gjør det den er ment å gjøre.

## Hvordan skrive tester i Python

For å skrive tester i Python, må du først importere "unittest" biblioteket. Deretter må du opprette en klasse som arver fra "unittest.TestCase". Innenfor denne klassen kan du definere ulike tester ved å bruke funksjonen "def test_metode_navn(self)". Inne i denne funksjonen kan du sjekke om de forventede resultatene samsvarer med de faktiske resultatene ved hjelp av assert-setninger.

```Python
import unittest

class TestKalkulator(unittest.TestCase):

    def test_addisjon(self):
        resultat = 1 + 2
        self.assertEqual(resultat, 3)
```

I dette eksempelet sjekker vi om resultatet av 1 + 2 faktisk er 3. Hvis ikke, vil testen feile og vi vet at det er noe galt med vår addisjonsfunksjon.

## Dypdykk i testing

Når du skriver tester, er det viktig å dekke alle scenarier og hjørnetilfeller for å sikre at koden din er robust. Du kan også bruke "mocking" for å teste funksjoner som er avhengige av eksterne ressurser eller nettverkskall. Det er også en god praksis å organisere tester i forskjellige filer og å kjøre dem automatisk før du publiserer koden din til produksjon.

## Se også

- [The Hitchhiker's Guide to Python Testing](https://docs.python-guide.org/writing/tests/)
- [Unittest Dokumentasjon](https://docs.python.org/3/library/unittest.html)
- [Testing in Python](https://realpython.com/python-testing/)