---
title:    "PHP: Testien kirjoittaminen"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Miksi

Testien kirjoittaminen on tärkeä osa PHP-ohjelmoinnin prosessia, sillä se auttaa varmistamaan koodin toimivuuden ja löytämään mahdollisia virheitä. Se myös helpottaa koodin ylläpitämistä ja kehittämistä pitkällä aikavälillä.

## Miten tehdä

Testien kirjoittaminen PHP:ssa on helppoa ja suoraviivaista. Ensimmäiseksi, varmista että [PHPUnit](https://phpunit.de/) on asennettuna ja käytössä. Sitten voit luoda yksinkertaisia testitapauksia seuraavasti:

```PHP
<?php
require_once 'Calculator.php';
use PHPUnit\Framework\TestCase;

class CalculatorTest extends TestCase
{
    public function testSum()
    {
        $calc = new Calculator();
        $this->assertEquals(4, $calc->sum(2, 2));
    }
}
```

Käynnistä testit suorittamalla komento `phpunit`. Näet tuloksen, joka näyttää siltä:

```
PHPUnit 9.0.0 by Sebastian Bergmann and contributors.

.                                                                   1 / 1 (100%)

Time: 00:00.002, Memory: 4.00 MB

OK (1 test, 1 assertion)
```

Hyväksytyn testituloksen tapauksessa näet viestin "OK", mutta mikäli testi epäonnistuu, saat virheilmoituksen ja tiedon siitä missä kohtaa testiä on ongelma.

## Syvällinen sukellus

Testien kirjoittamisen syvempi ymmärtäminen voi auttaa sinua kehittämään parempaa koodia. Testien avulla pystyt peittämään koko koodin eri osat ja varmistamaan, että ne toimivat yhteen. Ne myös auttavat sinua löytämään ja korjaamaan mahdollisia virheitä, ennenkuin ne pääsevät tuotantoon ja aiheuttavat ongelmia käyttäjille.

Varmista, että kirjoitat testitapauksia kaikille tärkeille osille koodiasi, jotta voit olla varma sen toimivuudesta ja vakaudesta.

## Katso myös

- [PHPUnitin dokumentaatio](https://phpunit.readthedocs.io/en/9.5/index.html)
- [The Art of Unit Testing: With Examples in PHP](https://www.amazon.com/Art-Unit-Testing-Examples-Net/dp/1617293705)
- [PHP Weekly](https://www.phpweekly.com/) - viikoittainen uutiskirje uusimmista PHP-aiheisista artikkeleista ja uutisista