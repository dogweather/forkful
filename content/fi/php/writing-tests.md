---
title:                "Testien kirjoittaminen"
date:                  2024-01-19
html_title:           "Arduino: Testien kirjoittaminen"
simple_title:         "Testien kirjoittaminen"

category:             "PHP"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why? - Mitä & Miksi?
Testaaminen tarkoittaa koodin toiminnan varmistamista automaattisesti. Koodaajat kirjoittavat testejä paikantaakseen bugeja, varmistuakseen ohjelman oikeasta toiminnasta päivitysten yhteydessä ja parantaakseen koodin laatua.

## How to: - Kuinka tehdään:
```PHP
<?php
// Yksinkertainen PHP-testi PHPUnitilla
use PHPUnit\Framework\TestCase;

// Testattava luokka
class Calculator {
    public function add($a, $b) {
        return $a + $b;
    }
}

// Testiluokka
class CalculatorTest extends TestCase {
    public function testAdd() {
        $calculator = new Calculator();
        $this->assertEquals(4, $calculator->add(2, 2));
    }
}

?>
```
Tulostus:
```
OK (1 test, 1 assertion)
```

## Deep Dive - Syväsukellus:
Alun perin testaus oli manuaalinen prosessi, mutta automatisoidut testit vakiintuivat 2000-luvulla. Vaihtoehtoja PHP:lle ovat esimerkiksi PHPUnit, Codeception ja PHPSpec. Hyvät testit ovat itsenäisiä, toistettavia ja kattavia. Ne testaavat sovelluksen odotettua käyttäytymistä ja paljastavat rikkinäisen koodin.

## See Also - Katso Myös:
- [PHPUnit](https://phpunit.de/)
