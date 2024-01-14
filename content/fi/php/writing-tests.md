---
title:                "PHP: Testien kirjoittaminen"
programming_language: "PHP"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/writing-tests.md"
---

{{< edit_this_page >}}

Miksi Testausten Kirjoittaminen on Tärkeää PHP-ohjelmoinnissa

Testausten kirjoittaminen on olennainen osa PHP-ohjelmointia. Se auttaa varmistamaan koodin laadun ja toimivuuden sekä vähentää mahdollisia bugeja ja virheitä. Lisäksi testien kirjoittaminen on myös hyvä tapa dokumentoida koodia ja auttaa uusien kehittäjien ymmärtämään sitä.

Miten

Testausten kirjoittaminen PHP:ssa on helppoa ja vaivatonta. Seuraavassa on muutama esimerkki, kuinka voit kirjoittaa testejä ohjelmallesi.

````PHP
public function testLaskePisteet()
{
  $lukujoukko = [5, 8, 3, 9];
  $pisteet = laskePisteet($lukujoukko);
  $this->assertEquals(25, $pisteet);
}
````

Yllä olevassa esimerkissä luodaan testi, joka varmistaa, että funktio `laskePisteet` laskee oikein pistemäärän annetusta lukujoukosta. Voit myös luoda useita testejä eri skenaarioille ja varmistaa, että ohjelmointisi toimii oikein kaikissa tilanteissa.

````PHP
public function testKorvaaKirjaimet()
{
  $merkkijono = "Hello World";
  $uusiMerkkijono = korvaaKirjaimet($merkkijono);
  $this->assertEquals("H3ll0 W0rld", $uusiMerkkijono);
}
````

Toisessa esimerkissä testataan funktiota `korvaaKirjaimet`, joka korvaa merkkijonossa olevat kirjaimet numeroina. Tekemällä erilaisia testejä eri skenaarioille voit varmistaa, että funktiosi toimii oikein kaikilla mahdollisilla syötteillä.

Syötteen antamisen lisäksi on myös tärkeää testata mahdollisia virhetilanteita ja niiden käsittelyä. Tämä auttaa varmistamaan, että ohjelmasi toimii oikein myös poikkeustilanteissa.

Syvempi Sukellus

Testien kirjoittamisessa on monia erilaisia tekniikoita ja työkaluja, jotka voivat auttaa sinua kirjoittamaan parempia testejä. Esimerkiksi PHPUnit on yleisesti käytetty työkalu PHP-testausten kirjoittamiseen. Lisäksi voit myös tutustua pohjatestaukseen, joka on tekniikka, jossa luodaan yksinkertaisia ja toimivia testejä jo ennen varsinaisen koodin kirjoittamista. Tämä auttaa varmistamaan koodin laadun jo varhaisessa vaiheessa.

Ulkoisten API:en ja kolmannen osapuolen kirjastojen testaaminen on myös tärkeää, jotta voit varmistaa niiden toimivuuden omissa projekteissasi. Niitä voi testata esimerkiksi käyttäen mock-kirjastoja, jotka luovat keinotekoisia vastauksia testien suorittamista varten.

Katso Myös

 - Lisätietoa PHPUnitista: https://phpunit.de/
 - Pohjatestauksen opas: https://www.guru99.com/string-mockup-test-design-technique.html
 - Mock-kirjastot: https://phpmock.readthedocs.io/en/latest/