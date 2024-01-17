---
title:                "Testien kirjoittaminen"
html_title:           "Go: Testien kirjoittaminen"
simple_title:         "Testien kirjoittaminen"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/writing-tests.md"
---

{{< edit_this_page >}}

# Mitä ja miksi?

Testien kirjoittaminen on prosessi, jolla ohjelman toiminnallisuutta varmistetaan koodin kirjoittamisen jälkeen. Testien avulla varmistetaan, että ohjelma toimii ja käyttäytyy odotetulla tavalla. Näin ohjelman suorittamista voidaan jatkuvasti testata ja varmistaa sen toimivuus.

# Miten:

Esimerkkejä testien kirjoittamisesta ja niiden tulosteista:

Go esimerkki:

```
func TestSum(t *testing.T) {
	total := Sum(4, 5)
	if total != 9 {
		t.Errorf("Expected result to be 9, but got %d instead", total)
	}
}

```

Tuloste:

```
--- FAIL: TestSum (0.00s)
    main_test.go:6: Expected result to be 9, but got 8 instead
FAIL
FAIL    example.com/testing    0.014s
```

# Syvempi sukellus:

Testien kirjoittaminen on tärkeä osa ohjelmointiprosessia, sillä se auttaa varmistamaan ohjelman luotettavuuden ja toimivuuden. Testien kirjoittamisella voi myös säästää aikaa ja vaivaa, sillä ne auttavat havaitsemaan mahdolliset virheet ja ongelmat jo koodaamisen aikana.

Muita vaihtoehtoja testien kirjoittamiseen ovat esimerkiksi manuaalinen testaus ja integraatiotestaus. Go tarjoaa testaamiseen erilaisia toimintoja, kuten testeittäin suorittamisen ja kattavuusraporttien luomisen.

Testien kirjoittamisessa käytetään yleensä yksikkö- ja integraatiotestejä. Yksikkötestit testaavat yksittäisiä koodilohkoja ja integraatiotestit testaavat eri osien välistä yhteistoimintaa.

# Katso myös:

- [Go testauksen viralliselta sivustolta](https://golang.org/pkg/testing/)
- [Martin Fowlerin artikkeli testien refactoroinnista](https://martinfowler.com/articles/refactoring-test-code.html)
- [Go testauksen aloittelijan opas](https://blog.alexellis.io/golang-writing-unit-tests/)