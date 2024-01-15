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

## Miksi

Go on moderni ohjelmointikieli, joka korostaa yksinkertaisuutta ja tehokkuutta. Kirjoittaessa testeja Go:lla, voit varmistaa koodisi toimivuuden ja luotettavuuden, mikä auttaa sinua luomaan laadukkaampia sovelluksia.

## Miten

Kirjoittaa testeja Go:lla on helppoa ja tehokasta. Tässä on esimerkki, miten voit luoda yksinkertaisen testin Go:n testilokerolle:

```Go
package test

import "testing"

func TestAddition(t *testing.T) {
	result := 2 + 2
	if result != 4 {
		t.Error("Expected 4, got", result)
	}
}
```

Tässä esimerkissä luodaan yksinkertainen funktio, joka testaa kahden numeron yhteenlaskua. Testilokeron avulla voit määrittää odotetun tuloksen ja varmistaa, että funktiosi palauttaa oikean arvon.

Testilokeron käyttöönotto vaatii vain yhden tuontilausekkeen "testing", ja sen avulla voit käyttää monia erilaisia testaukseen liittyviä toimintoja, kuten "t.Error()", joka kertoo meille, jos testi epäonnistuu. Testilokeroa käytetään myös "go test" -komennolla, joka ajaa kaikki testisi ja ilmoittaa, jos jokin niistä epäonnistuu.

## Syvä sukellus

Testaaminen on tärkeä osa ohjelmointia ja voi auttaa sinua löytämään virheitä ja bugeja koodistasi varhaisessa vaiheessa, mikä säästää aikaa ja vaivaa myöhemmin. Go käyttää "go test" -toimintoa, joka sisältää sisäänrakennetut testaukseen liittyvät toiminnot, joten sinun ei tarvitse etsiä ja tuoda kirjastoja tai ohjelmia, jotka auttavat sinua testauksessa.

Lisäksi Go:lla on käytettävissä myös muita testaukseen liittyviä työkaluja, kuten "go cover", joka auttaa seuraamaan testien kattavuutta ja löytämään osat koodista, jotka eivät ole tarpeeksi testattuja.

## Katso myös

- [Virallinen Go:n dokumentaatio testauksesta](https://golang.org/pkg/testing/)
- [Go Bootcamp -sarja, joka sisältää paljon tietoa Go:n testauksesta](http://www.golangbootcamp.com/book/testing)
- [Go:n viralliset esimerkit testaamisesta GitHubissa](https://github.com/golang/go/wiki/LearnTests)