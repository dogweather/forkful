---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:31:12.335764-07:00
description: "\xC9crire des tests en programmation implique de cr\xE9er et d'ex\xE9\
  cuter des scripts qui v\xE9rifient que le code se comporte comme pr\xE9vu dans diverses\
  \ conditions.\u2026"
lastmod: '2024-03-13T22:44:57.882433-06:00'
model: gpt-4-0125-preview
summary: "\xC9crire des tests en programmation implique de cr\xE9er et d'ex\xE9cuter\
  \ des scripts qui v\xE9rifient que le code se comporte comme pr\xE9vu dans diverses\
  \ conditions."
title: "R\xE9daction de tests"
weight: 36
---

## Quoi & Pourquoi ?
Écrire des tests en programmation implique de créer et d'exécuter des scripts qui vérifient que le code se comporte comme prévu dans diverses conditions. Les programmeurs le font pour assurer la qualité, prévenir les régressions et faciliter le refactoring sûr, ce qui est crucial pour maintenir une base de code saine, évolutive et sans bugs.

## Comment faire :
### PHP Natif – PHPUnit
Un outil largement utilisé pour les tests en PHP est PHPUnit. Installez-le via Composer :
```bash
composer require --dev phpunit/phpunit ^9
```

#### Écrire un test simple :
Créez un fichier `CalculatorTest.php` dans un répertoire `tests` :
```php
use PHPUnit\Framework\TestCase;

// En supposant que vous avez une classe Calculator qui additionne des nombres
class CalculatorTest extends TestCase
{
    public function testAdd()
    {
        $calculator = new Calculator();
        $this->assertEquals(4, $calculator->add(2, 2));
    }
}
```
Exécutez les tests avec :
```bash
./vendor/bin/phpunit tests
```

#### Exemple de sortie :
```
PHPUnit 9.5.10 par Sebastian Bergmann et contributeurs.

.                                                                   1 / 1 (100%)

Temps : 00:00.005, Mémoire : 6.00 MB

OK (1 test, 1 assertion)
```

### Bibliothèques tierces – Mockery
Pour des tests complexes, notamment le mocking d'objets, Mockery est un choix populaire.

```bash
composer require --dev mockery/mockery
```

#### Intégrer Mockery avec PHPUnit :
```php
use PHPUnit\Framework\TestCase;
use Mockery as m;

class ServiceTest extends TestCase
{
    public function tearDown(): void
    {
        m::close();
    }

    public function testServiceCallsExternalService()
    {
        $externalServiceMock = m::mock(ExternalService::class);
        $externalServiceMock->shouldReceive('process')->once()->andReturn('mocked result');

        $service = new Service($externalServiceMock);
        $result = $service->execute();

        $this->assertEquals('mocked result', $result);
    }
}
```
Pour exécuter, utilisez la même commande PHPUnit qu'au-dessus. Mockery permet des objets mock expressifs et flexibles, facilitant le test des interactions complexes au sein de votre application.
