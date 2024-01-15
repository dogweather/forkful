---
title:                "Generering av tilfeldige tall"
html_title:           "PHP: Generering av tilfeldige tall"
simple_title:         "Generering av tilfeldige tall"
programming_language: "PHP"
category:             "PHP"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hvorfor
Å generere tilfeldige tall er en vanlig oppgave i mange programmer og kan være nyttig i ulike situasjoner. Dette kan inkludere å opprette tilfeldige brukernavn og passord, trekke en vinner i en konkurranse, eller generere testdata for programmeringstester.

## Hvordan
For å generere tilfeldige tall i PHP kan du bruke funksjonen `rand()`. Denne funksjonen tar to parametere, et minimumsnummer og et maksimumsnummer, og returnerer et tilfeldig tall innenfor dette området. For eksempel, hvis du vil generere et tilfeldig tall mellom 1 og 10, kan du bruke følgende kode:
```PHP
$tilfeldig_tall = rand(1, 10);
echo $tilfeldig_tall; //vil skrive ut et tilfeldig tall mellom 1 og 10
```
Du kan også bruke `mt_rand()`-funksjonen, som er en mer avansert versjon av `rand()`, og gir bedre tilfeldighet. Denne funksjonen tar to parametere, et minimumsnummer og et maksimumsnummer, og fungerer på samme måte som `rand()`. Et eksempel på bruken av `mt_rand()`:
```PHP
$tilfeldig_tall = mt_rand(20, 50);
echo $tilfeldig_tall; //vil skrive ut et tilfeldig tall mellom 20 og 50
```

## Deep Dive
PHP har også en funksjon som kan generere tilfeldige tall basert på en forhåndsbestemt rekkefølge, kalt `srand()`. Ved å bruke denne funksjonen kan du kontrollere rekkefølgen av tilfeldige tall som blir generert. For eksempel, hvis du vil generere de samme tilfeldige tallene i flere runder av koden din, kan du bruke `srand()` i begynnelsen av koden din og gi den samme parameteren hver gang. Dette vil sikre at den samme tilfeldige sekvensen blir generert i hver runde.

## Se også
- [PHP rand() function](https://www.php.net/manual/en/function.rand.php)
- [PHP mt_rand() function](https://www.php.net/manual/en/function.mt-rand.php)
- [PHP srand() function](https://www.php.net/manual/en/function.srand.php)