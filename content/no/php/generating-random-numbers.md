---
title:    "PHP: Generering av tilfeldige tall"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Hvorfor
Generering av tilfeldige tall er en viktig del av mange programmeringsoppgaver. Dette kan være nyttig for å lage spill, simuleringer, eller for kryptering av data. Å forstå hvordan man kan generere tilfeldige tall i PHP kan være en verdifull ferdighet å ha for enhver programmerer.

## Hvordan 
Generering av tilfeldige tall i PHP kan enkelt gjøres ved hjelp av funksjonen `rand()`. Denne funksjonen tar to argumenter, hvor det første angir minimumsverdien og det andre angir maksimumsverdien, for å generere et tall innenfor det gitt intervallet. For eksempel, hvis vi ønsker å generere et tilfeldig tall mellom 1 og 10, kan vi bruke følgende kode:

```
<?php
echo rand(1, 10); //output: et tilfeldig tall mellom 1 og 10
?>
```

For å generere flere tilfeldige tall, kan vi bruke en løkke og lagre tallene i en array for enkel tilgang senere.

```
<?php
$tilfeldige_tall = array(); //array for å lagre tilfeldige tall
for ($i=0; $i<10; $i++) { //genererer 10 tall
    $tilfeldige_tall[] = rand(1, 100); //hvert tall legges til i arrayen
}
print_r($tilfeldige_tall); //printer ut alle tall i arrayen
?>
```

Dette vil gi en output som ligner på følgende:

```
Array
(
    [0] => 46
    [1] => 21
    [2] => 93
    [3] => 5
    [4] => 89
    [5] => 3
    [6] => 64
    [7] => 77
    [8] => 19
    [9] => 11
)
```

## Dypdykk
Men hva skjer egentlig bak kulissene når vi genererer tilfeldige tall i PHP? I virkeligheten bruker PHP en algoritme som bruker en såkalt "seed" (frø) for å produsere et tilfeldig tall. Denne seeden er vanligvis tiden på klokken når funksjonen blir kalt, og det er derfor vi får forskjellige tall hver gang vi kaller på `rand()`.

I tillegg til `rand()`, har PHP også en mer avansert funksjon for generering av tilfeldige tall - `mt_rand()`. Denne funksjonen er kjent for å gi jevnere distribusjon av tall og er derfor å foretrekke i situasjoner hvor nøyaktigheten til tallene er viktig.

## Se Også
- [PHP rand() function documentation](https://www.php.net/manual/en/function.rand.php)
- [PHP mt_rand() function documentation](https://www.php.net/manual/en/function.mt-rand.php)
- [Generating Random Numbers in PHP](https://www.geeksforgeeks.org/generating-random-numbers-in-php/)