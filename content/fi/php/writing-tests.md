---
title:    "PHP: Testien kirjoittaminen"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/php/writing-tests.md"
---

{{< edit_this_page >}}

## Miksi

Kirjoittaminen testejä on tärkeä osa PHP-ohjelmointia, sillä se auttaa varmistamaan ohjelman laadun ja vähentää mahdollisia virheitä järjestelmässä.

## Kuinka kirjoittaa testejä PHP:lla

Testien kirjoittaminen PHP:lla on helppoa, ja se tehdään yleensä jokaisen toiminnon yhteydessä. Alla on esimerkki siitä, kuinka voit kirjoittaa yksinkertaisen testin funktiolle, joka laskee kahden luvun summan.

```PHP
function sum($a, $b){
  return $a + $b;
}

echo sum(2, 3); // Output: 5
```

Nyt voimme kirjoittaa yksinkertaisen testin, joka testaa että funktio palauttaa odotetun arvon.

```PHP
// Tarkistetaan että funktio palauttaa oikean summan
function test_sum(){
  $expected = 5;
  $actual = sum(2, 3);
  if ($expected === $actual) {
    echo "Testi onnistui! Summa on 5.";
  } else {
    echo "Testi epäonnistui! Summa ei vastaa odotettua arvoa.";
  }
}

test_sum(); // Output: Testi onnistui! Summa on 5.
```

## Syvempi sukellus testien kirjoittamiseen

Testien kirjoittaminen auttaa paitsi varmistamaan ohjelman toimivuuden, myös helpottaa koodin korjaamista ja ylläpitoa tulevaisuudessa. Hyvä käytäntö on kirjoittaa testejä jokaiselle toiminnolle ja varmistaa, että testit kattavat kaikki mahdolliset tilanteet ja antavat oikeat tulokset.

Voit myös käyttää erilaisia testikehyksiä, kuten PHPUnit, joka tarjoaa lisäominaisuuksia testien kirjoittamiseen ja suorittamiseen.

## Katso myös

- [PHPUnit](https://phpunit.de/)
- [PHP-testausopas](https://www.php.net/manual/en/writing-tests.php)
- [Testien kirjoittaminen PHP:lla](https://www.tutorialspoint.com/phpunit/phpunit_writing_tests.htm)