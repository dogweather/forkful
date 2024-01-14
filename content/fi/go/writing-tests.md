---
title:    "Go: Testien kirjoittaminen"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Miksi

Testien kirjoittaminen ei ehkä kuulosta houkuttelevalta osalta ohjelmoinnin prosessia, mutta se on tärkeä askel kohti virheetöntä koodia. Testit auttavat löytämään bugeja ja varmistamaan, että koodi toimii halutulla tavalla.

## Miten

Go-kielessä testien kirjoittaminen on helppoa ja suoraviivaista. Testit kirjoitetaan samassa paketissa kuin itse koodi ja niiden tulee olla tiedostossa, joka päättyy "_test.go" -nimeen. Alla on esimerkki testin kirjoittamisesta, jossa testataan yksinkertaista "add" -funktiota.

```Go
func add(x, y int) int {
    return x + y
}

func TestAdd(t *testing.T) {
    result := add(2, 3)
    if result != 5 {
        t.Errorf("Expected 5, got %d", result)
    }
}
```

Suorittaessa testit komennolla "go test" näemme seuraavan tulosteen:

```
--- FAIL: TestAdd (0.00s)
    main_test.go:10: Expected 5, got 7
    FAIL
    exit status 2
    FAIL    _/path/to/package    0.001s
```

Tulosteen ensimmäinen rivi kertoo, että testi epäonnistui ja seuraavat rivit näyttävät, miksi. Virheilmoitus kertoo, että odotettiin 5, mutta saatiinkin 7.

## Syvemmälle

Testit auttavat myös kirjoittamaan selkeitä ja huolellisesti suunniteltuja funktioita. Niiden avulla voi simuloida erilaisia tilanteita ja varmistaa, että koodi toimii odotetusti. On myös tärkeää testata erilaisia reunatapauksia, kuten virheelliset syötteet, jotta koodi kestää kaikki mahdolliset käyttötapaukset.

Go-kielessä on myös mahdollista luoda benchmark-testejä, joilla voidaan mitata koodin suorituskykyä ja etsiä mahdollisia pullonkauloja. Nämä testit voivat auttaa parantamaan koodin tehokkuutta ja optimoimaan sen suoritusta.

## Katso myös

- [Go testing -pakettien virallinen dokumentaatio](https://golang.org/pkg/testing/)
- [Hienot Go testiframeworkit, joita sinun pitäisi käyttää](https://blog.alexellis.io/golang-writing-unit-tests/)
- [Esimerkkikoodi: Simple Calculator -sovellus ja siihen liittyvät testit](https://github.com/exampleuser/calculator)