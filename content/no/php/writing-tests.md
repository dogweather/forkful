---
title:                "Skrive tester"
html_title:           "PHP: Skrive tester"
simple_title:         "Skrive tester"
programming_language: "PHP"
category:             "PHP"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/writing-tests.md"
---

{{< edit_this_page >}}

## Hvorfor?

Å skrive tester i programmering er en viktig praksis for å sikre at koden vår fungerer som den skal. Ved å skrive tester kan vi fange feil og feil tidlig i utviklingsprosessen, noe som sparer tid og hindrer potensielle problemer i å dukke opp senere.

## Hvordan

For å skrive tester i PHP, trenger vi et testrammeverk. Et populært rammeverk er PHPUnit, som er et gratis og open-source testrammeverk utviklet spesielt for PHP-kode.

For å installere PHPUnit, kan du følge disse trinnene:

1. Åpne terminalen og naviger til prosjektmappen din.
2. Kjør kommandoen ``composer require phpunit/phpunit`` for å installere PHPUnit.
3. Lage en testfil ved å opprette en ``tests`` -mappe og en ny PHP-fil i den.
4. Importer PHPUnit ved hjelp av ``require_once`` -funksjonen.
5. Skriv tester ved å bruke PHPUnit-funksjoner og asserter for å sjekke forventet resultat av din PHP-kode.
6. Kjør testene ved å kjøre kommandoen ``./vendor/bin/phpunit [testfilnavn]`` i terminalen.

Her er et eksempel på hvordan en test kan se ut:

```PHP
<?php

require_once 'vendor/autoload.php';

// Importerer klassen vi vil teste
use App\Kalkulator;

class KalkulatorTest extends PHPUnit\Framework\TestCase
{
    // Tester addisjonsfunksjonen
    public function testAddisjon()
    {
        $resultat = Kalkulator::addisjon(2, 4); // Resultatet bør være 6
        $this->assertEquals(6, $resultat); // Sjekker om resultatet er 6
    }
}
```

Output fra testen vil være:

```
PHPUnit 9.5.4 by Sebastian Bergmann and contributors.

.                                                                   1 / 1 (100%)

Time: 00:00.032, Memory: 4.00 MB

OK (1 test, 1 assertion)
```

## Deep Dive

Det er viktig å skrive gode tester for å sikre at de er pålitelige og effektive. Her er noen tips for å skrive gode tester i PHPUnit:

- Sørg for å navngi testene dine på en forståelig måte, så det er lettere å forstå hva de tester.
- Bruk asserter som ``assertEquals``, ``assertTrue`` og ``assertFalse`` for å sjekke forventede resultater.
- Test forskjellige kombinasjoner av input for å sikre at koden fungerer for alle tilfeller.
- Unngå å teste avhengigheter som databaser og nettverkskall, da dette kan føre til unødvendige kompleksiteter og økt testtid.

## Se også

- [PHPUnit hjemmeside](https://phpunit.de/)
- [PHPUnit-dokumentasjon](https://phpunit.readthedocs.io/en/9.5/index.html)
- [PHPUnit-tutorials på Tutsplus](https://code.tutsplus.com/series/unit-testing-with-phpunit--cms-1191)