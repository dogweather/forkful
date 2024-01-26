---
title:                "Pisanie testów"
html_title:           "Bash: Pisanie testów"
simple_title:         "Pisanie testów"
programming_language: "PHP"
category:             "PHP"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/writing-tests.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Pisanie testów to sprawdzanie kodu w różnych scenariuszach, by mieć pewność, że działa jak należy. Programiści robią to, by uniknąć błędów, upewnić się, że nowe zmiany nie psują istniejących funkcjonalności i by móc bez obaw refaktoryzować kod.

## Jak to zrobić:
Będziemy używać PHPUnit – standardowego narzędzia do testów w PHP. Poniżej prosty test sprawdzający, czy nasza funkcja do sumowania liczb działa prawidłowo:

```PHP
<?php
use PHPUnit\Framework\TestCase;

class SumTest extends TestCase
{
    public function testThatSumsRight()
    {
        require 'Sum.php'; // Załóżmy, że tutaj mamy naszą funkcję do sumowania
        $result = sum(2, 3);
        $this->assertEquals(5, $result);
    }
}
?>
```
Załóżmy, że w `Sum.php` mamy:
```PHP
<?php

function sum($a, $b) {
    return $a + $b;
}

?>
```
Uruchomienie tego testu da nam zieloną krzaczkę – znak, że test przeszedł pomyślnie.

## W głębi tematu:
Testy jednostkowe w PHP zyskały na popularności dzięki narzędziu xUnit. PHPUnit, który jest dzisiejszym standardem, został zainspirowany tym rozwiązaniem. Oprócz PHPUnit istnieją także inne frameworki jak Codeception czy phpspec. Ważne w testowaniu jest zrozumienie koncepcji testu jednostkowego, integracyjnego, systemowego i akceptacyjnego. Dodatkowo, implementacja Continuous Integration (CI) z automatyzacją testów pozwala na szybką i skuteczną weryfikację kodu.

## Zobacz też:
- Oficjalna strona PHPUnit: [https://phpunit.de/](https://phpunit.de/)
- Wzorce testów xUnit: [https://en.wikipedia.org/wiki/XUnit](https://en.wikipedia.org/wiki/XUnit)
