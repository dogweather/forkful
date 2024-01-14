---
title:    "PHP: Generering av tilfeldige tall"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/php/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hvorfor

Mange programmerere kan føle at bruk av tilfeldige tall er unødvendig, men det finnes faktisk flere grunner til å inkludere denne funksjonen i koden din. Først og fremst kan tilfeldige tall brukes til å generere ulike scenarier og testing av edge-cases. Dette kan være nyttig når du ønsker å teste hvordan koden din vil oppføre seg i ulike situasjoner. I tillegg kan tilfeldige tall brukes til å skape en følelse av tilfeldighet i et spill eller en annen applikasjon.

## Slik gjør du det

Det er flere måter å generere tilfeldige tall i PHP. En enkel måte å gjøre det på er ved å bruke funksjonen `rand()`, som tar to argumenter som definerer området hvor de tilfeldige tallene skal genereres fra. For eksempel, for å generere et tilfeldig tall mellom 1 og 10 kan du bruke følgende kode:

```PHP
$tilfeldig_tall = rand(1, 10);
echo $tilfeldig_tall; // Utskrift: et tilfeldig tall mellom 1 og 10
```

For å generere et tilfeldig desimaltall, kan du bruke funksjonen `mt_rand()`, som også tar to argumenter, men har et større område for tilfeldige tall.

```PHP
$tilfeldig_desimal = mt_rand(1, 100) / 10;
echo $tilfeldig_desimal; // Utskrift: et tilfeldig desimaltall mellom 0.1 og 10
```

Det finnes også andre funksjoner for å generere tilfeldige tall i PHP, som `random_int()` og `random_bytes()`, som er mer sikre og anbefales for kryptografiske formål.

## Dykk dypere

Det er viktig å merke seg at funksjonene `rand()` og `mt_rand()` ikke er helt tilfeldige og kan føre til forutsigbare resultater. For å unngå dette, er det viktig å bruke en tilfeldig startverdi før du genererer tilfeldige tall. I tillegg kan man også bruke funksjonen `srand()` for å sette en startverdi.

Du kan også bruke funksjonen `shuffle()` for å tilfeldig omorganisere elementer i en array i en tilfeldig rekkefølge.

## Se også

- [PHPs offisielle dokumentasjon for tilfeldige tall](https://www.php.net/manual/en/function.rand.php)
- [Tilfeldig tallgenerator fra random.org](https://www.random.org/)