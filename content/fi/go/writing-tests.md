---
title:    "Go: Testien kirjoittaminen"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/go/writing-tests.md"
---

{{< edit_this_page >}}

## Miksi

Testien kirjoittaminen on tärkeä osa ohjelmistokehitystä, joka auttaa varmistamaan koodin toiminnallisuuden ja luotettavuuden. Ilman testejä koodin hankalat bugin metsästys ja korjaaminen voi viedä paljon aikaa ja resursseja. Testien avulla voit myös helposti havaita regressiot, eli aiemmin toimineen koodin rikkoutumisen uusien muutosten jälkeen.

## Miten tehdä

Testien kirjoittaminen Go-kielellä on helppoa ja suoraviivaista. Seuraavassa on muutamia esimerkkejä siitä, miten voit aloittaa testien kirjoittamisen omissa ohjelmissasi.

### Yksikkötestit

Aloita lisäämällä testikoodi samaan pakettiin kuin ohjelman koodi ja nimeämällä tiedoston perimän "test" lisäten loppuun "_test.go". Tämä auttaa Go-kääntäjää tunnistamaan tiedoston testejä varten.

Seuraavassa on yksinkertainen esimerkki lisäämällä yksikkötesti function tai metodi luokkaan. Koodiblokkiin on lisätty myös odotettu testin tulos:

```Go
package main

import (
	"testing"
)

func Add(x, y int) int {
	return x + y
}

func TestAdd(t *testing.T) {
	result := Add(2, 3)
	expected := 5

	if result != expected {
		t.Errorf("Expected result to be %d, but got %d instead.", expected, result)
	}
}
```

Voit ajaa testit komennolla `go test` kansiossa, jossa testikoodi sijaitsee.

### Benchmark-testit

Voit myös tehdä benchmark-testejä Go-kielellä, jotta voit vertailla funktion suorituskykyä. Benchmark-koodin tulee olla nimetty perimän "test" ja loppuun "_bench.go".

Tässä esimerkissä verrataan slice-apufunktioiden `append()` ja `copy()` suorituskykyä. Benchmark-koodiblokki kuvaa, kuinka monta kertaa kumpikin funktio suoritettiin 1000 kertaa.

```Go
package main

import "testing"

var s []int

func BenchmarkAppend(b *testing.B) {
    for i := 0; i < b.N; i++ {
        s = append(s, i)
    }
}

func BenchmarkCopy(b *testing.B) {
    dest := make([]int, b.N)
    for i := 0; i < b.N; i++ {
        copy(dest[i:], s)
    }
}
```

Benchmark-testit voidaan ajaa lisäämällä `-bench=.` komennon perään. 

## Syvempi sukellus

Testien kirjoittamisessa on tärkeää huomioida muutamia asioita. Ensinnäkin, testien tulee olla riittävän kattavia ja testata kaikki mahdolliset skenaariot. Toiseksi, testikoodin tulee olla helppolukuista ja ymmärrettävää, jotta sen pohjalta voidaan helposti havaita ja korjata mahdolliset virheet.

Hyvä tapa aloittaa testien kirjoittaminen on käyttää testilähtökohtaa (test-driven development), jossa ensin kirjoitetaan testikoodi ja sen perusteella toteutetaan ohjelmakoodi. Tällä tavoin varmistetaan, että testit ovat tarpeeksi kattavia ja toimivat oikein.

## Katso myös

- [The Go Blog: The Go Programming Language](https://blog.golang.org/)
- [Go Wiki: Writing Tests](https://github.com/golang/go/wiki/TableOfContents#testing)
- [Testing in Go: Tools and Techniques](https://medium.com/rungo/unit-testing-made-easy-in-go-25077669318)