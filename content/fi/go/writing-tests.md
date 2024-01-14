---
title:                "Go: Testien kirjoittaminen"
programming_language: "Go"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/writing-tests.md"
---

{{< edit_this_page >}}

## Miksi

On tärkeää, että ohjelmistokehittäjät kirjoittavat testejä ohjelmakoodiinsa. Testien avulla voidaan varmistaa, että koodi toimii oikein ja vähentää bugeja ohjelmassa.

## Kuinka

Testien kirjoittaminen Go-kielellä on helppoa ja selkeää. Alla on esimerkkejä ja tulosteita koodinpätkässä "```Go ... ```".

```Go
// Esimerkki yksinkertaisesta testistä
func TestCalculateTotal(t *testing.T) {
    total := calculateTotal(50, 10)

    if total != 60 {
        t.Errorf("Odottamaton tulos! Halusimme 60, mutta saimme %f", total)
    }
}

// Tuloste:
// --- FAIL: TestCalculateTotal (0.00s)
//     main_test.go:7: Odottamaton tulos! Halusimme 60, mutta saimme 55

```

```Go
// Esimerkki testifunktion käyttämisestä
func TestMultiply(t *testing.T) {
    testCases := []struct {
        input1 int
        input2 int
        expectedOutput int
    }{
        {2, 5, 10},
        {10, 0, 0},
        {-3, 7, -21},
    }

    for _, tc := range testCases {
        output := multiply(tc.input1, tc.input2)

        if output != tc.expectedOutput {
            t.Errorf("Odottamaton tulos! Halusimme %d, mutta saimme %d", tc.expectedOutput, output)
        }
    }
}

// Tuloste:
// --- PASS: TestMultiply (0.00s)
```

## Syvempää tietoa

Testejä voi kirjoittaa monella eri tavalla ja testausstrategia kannattaa valita projektikohtaisesti. Alla on muutama hyvä resurssi testien kirjoittamiseen Go-kielellä:

- [Go:n virallinen dokumentaatio testauksesta](https://golang.org/pkg/testing/)
- [GoTest-sivusto, jossa on paljon vinkkejä ja esimerkkejä testien kirjoittamisesta Go-kielellä](https://gotest.tools/)
- [Go-koodin testauksen paras käytäntö -artikkeli](https://medium.com/@matryer/5-simple-tips-and-tricks-for-writing-unit-tests-in-golang-619653f90742)

## Katso myös

- [Mitä ovat yksikkötestit ja miksi niitä tarvitsemme?](https://www.oreilly.com/library/view/97-things-every/9780596809495/ch01.html)
- [Toinen blogipostaus testien kirjoittamisesta Go-kielellä](https://medium.com/@simonritchie/why-is-writing-unit-tests-so-important-yc-q-of-the-week-d41c1cf467d6)
- [Go-koodin testauksen esimerkkien hakemisto GitHubissa](https://github.com/golang-standards/project-layout/tree/master/testing)