---
title:                "Rédaction de tests"
html_title:           "Arduino: Rédaction de tests"
simple_title:         "Rédaction de tests"
programming_language: "PHP"
category:             "PHP"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/writing-tests.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?
Écrire des tests, c'est créer des scénarios pour automatiquement vérifier que le code fonctionne comme prévu. Les développeurs font cela pour assurer la qualité, détecter des bugs tôt et faciliter la maintenance.

## Comment faire :
Utilise PHPUnit pour écrire des tests. Installe-le via Composer avec `composer require --dev phpunit/phpunit`. Voici un exemple simplifié :

```PHP
<?php
use PHPUnit\Framework\TestCase;

class StackTest extends TestCase
{
    public function testPushAndPop()
    {
        $stack = [];
        $this->assertSame(0, count($stack));

        array_push($stack, 'foo');
        $this->assertSame('foo', $stack[count($stack)-1]);
        $this->assertSame(1, count($stack));

        $this->assertSame('foo', array_pop($stack));
        $this->assertSame(0, count($stack));
    }
}
```
Lancer les tests avec `./vendor/bin/phpunit tests`.

## Plongée en profondeur :
Historiquement, PHPUnit s'est inspiré par JUnit, pratiquant le TDD (Test-Driven Development) à l'origine de Java. Des alternatives existent, comme PHPBehat pour le BDD (Behavior-Driven Development) ou PHPSpec. Les tests unitaires avec PHPUnit nécessitent que chaque fonctionnalité ait un test correspondant, isolant les composants pour vérifier leur bon fonctionnement indépendamment.

## Voir aussi :
- Documentation de PHPUnit : [phpunit.de/manual/current/en/index.html](https://phpunit.de/manual/current/en/index.html)
- TDD sur Wikipedia: [fr.wikipedia.org/wiki/Test_driven_development](https://fr.wikipedia.org/wiki/Test_driven_development)
- Tutoriel PHP Test : [phptherightway.com/#testing](https://phptherightway.com/#testing)
