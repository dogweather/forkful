---
title:                "PHP: Skrive tester"
programming_language: "PHP"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/writing-tests.md"
---

{{< edit_this_page >}}

# Hvorfor
Å skrive tester er en viktig del av programmering fordi det hjelper deg med å identifisere og løse feil og sørger for at koden din fungerer som den skal. Det kan også bidra til å forebygge fremtidige problemer og gjøre det enklere å vedlikeholde koden.

# Hvordan
For å skrive tester i PHP, kan du bruke et rammeverk som PHPUnit. Her er et eksempel på hvordan du kan skrive en enkel test som sjekker om et gitt tall er større enn 10:

```PHP
<?php
use PHPUnit\Framework\TestCase;

class TestEksempel extends TestCase
{
    public function testSjekkTall()
    {
        $tall = 12;
        $this->assertGreaterThan(10, $tall);
    }
}
```

Når du kjører denne testen, vil du få en output som sier "OK (1 test, 1 assertion)". Dette betyr at testen har passert og tallet er faktisk større enn 10.

# Dypdykk
Når du skriver tester, er det viktig å tenke på ulike scenarioer og dekke alle mulige utfall. Dette kan være f.eks. å teste funksjoner med ulike inputs, håndtere feil og unntak, og sjekke hvordan koden din oppfører seg med ulike versjoner av PHP.

En annen god praksis når du skriver tester er å skrive dem før du begynner å kode. Dette vil hjelpe deg med å definere hva du forventer at koden skal gjøre, og du kan bruke testene som en guide mens du utvikler.

# Se også
- [PHPUnit dokumentasjon](https://phpunit.readthedocs.io/en/9.3/)
- [Beste praksis for å skrive tester i PHP](https://phptherightway.com/#testing)
- [Hvordan bruke PHP CodeSniffer for å sikre koden din](https://www.keycdn.com/blog/php-codesniffer)

Med disse tipsene bør du være godt rustet til å komme i gang med å skrive tester i PHP. Lykke til!