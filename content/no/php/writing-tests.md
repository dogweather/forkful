---
title:                "Skriving av tester"
html_title:           "PHP: Skriving av tester"
simple_title:         "Skriving av tester"
programming_language: "PHP"
category:             "PHP"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/writing-tests.md"
---

{{< edit_this_page >}}

## Hva og Hvorfor? 
Writing tests er en viktig del av programmering som hjelper utviklere med å sikre at koden de skriver fungerer som forventet. Det innebærer å skrive automatiserte tester for å sjekke at koden oppfører seg som den skal i ulike situasjoner. Dette sikrer bedre kvalitet og reduserer risikoen for feil og konflikter i koden.

## Hvordan:
Den enkleste måten å skrive tester på i PHP er å bruke et test-rammeverk som PHPUnit. Her kan du definere tester ved hjelp av assert-statements som sjekker om en forventet verdi samsvarer med den faktiske verdien som returneres fra koden. Se et eksempel nedenfor:

```PHP 
// Definerer en klasse for å teste
class TestKlasse {
    // Eksempel på en funksjon du ønsker å teste
    public function leggTil($a, $b) {
        return $a + $b;
    }
}

// Oppretter et nytt testobjekt
$test = new TestKlasse();

// Tester funksjonen og sammenligner forventet og faktisk verdi
assert($test->leggTil(2, 3) === 5);
```

Når du kjører testen, vil PHPUnit automatisk sjekke om funksjonen leggTil returnerer riktig verdi.

## Dypdykk:
Å skrive tester har blitt en viktig praksis i moderne programmering, spesielt innenfor utvikling av større og mer komplekse systemer. Ved hjelp av automatiserte tester kan man raskt sjekke om endringer i koden ødelegger funksjonaliteten, og dermed redusere tiden og kostnadene forbundet med feilsøking.

Det finnes ulike test-rammeverk og biblioteker tilgjengelig for å skrive tester i PHP, inkludert PHPUnit, Behat og Codeception. Disse tilbyr forskjellige funksjoner og tilnærminger til testing, så det kan være lurt å utforske og finne det som passer best for prosjektet ditt.

Når det gjelder implementering, bør det nevnes at det er en god praksis å skrive tester før man skriver kode, da det kan hjelpe deg med å presisere hva koden skal gjøre og bidra til å unngå unødvendig kompleksitet.

## Se også:
- [PHPUnit dokumentasjon](https://phpunit.de/documentation.html)
- [Behat test-rammeverk](https://behat.org/)
- [Codeception PHP-testingbibliotek](https://codeception.com/)