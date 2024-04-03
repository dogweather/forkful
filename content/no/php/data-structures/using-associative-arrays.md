---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:12:45.883881-07:00
description: "Assosiative tabeller i PHP er som superladde lister der hvert element\
  \ kan n\xE5s ved hjelp av en menneskeleselig n\xF8kkel i stedet for bare tall. Programmerere\u2026"
lastmod: '2024-03-13T22:44:40.878901-06:00'
model: gpt-4-0125-preview
summary: "Assosiative tabeller i PHP er som superladde lister der hvert element kan\
  \ n\xE5s ved hjelp av en menneskeleselig n\xF8kkel i stedet for bare tall."
title: Bruke associative tabeller
weight: 15
---

## Hvordan:
I PHP er oppretting og bruk av assosiative tabeller enkelt. Her er en rask oversikt:

```PHP
<?php
// Oppretter en assosiativ tabell
$person = array(
    "name" => "John Doe",
    "age" => 30,
    "email" => "john@example.com"
);

// Eller, den korte tabellsyntaksen
$person = [
    "name" => "John Doe",
    "age" => 30,
    "email" => "john@example.com"
];

// Å få tilgang til verdier ved bruk av nøkler
echo "Navn: " . $person["name"] . "\n";
echo "Alder: " . $person["age"] . "\n";
echo "E-post: " . $person["email"] . "\n";

// Modifiserer en verdi
$person["age"] = 31;

// Legger til et nytt nøkkel-verdi par
$person["country"] = "USA";

// Itererer over en assosiativ tabell
foreach ($person as $key => $value) {
    echo $key . ": " . $value . "\n";
}

// Utdata
// Navn: John Doe
// Alder: 31
// E-post: john@example.com
// land: USA
?>
```

Merk hvordan nøkler kan være hvilken som helst streng, noe som tillater deg å få tilgang til elementer ved hjelp av disse nøklene i stedet for numeriske indekser, som kan være mindre meningsfulle og vanskeligere å huske.

## Dypdykk
Assosiative tabeller i PHP implementeres internt ved hjelp av hashtabeller som gir veldig rask tilgang til elementer etter nøkkel, noe som gjør dem svært effektive for mange oppgaver. Denne effektiviteten, kombinert med deres brukervennlighet, gjør assosiative tabeller til en hjørnestein i PHP-programmering.

Historisk sett har PHPs tabeller (både indekserte og assosiative) vært utrolig fleksible, noe som har gjort det mulig for dem å fungere som lister, stabler, køer og mer. Imidlertid kan denne fleksibiliteten noen ganger føre til mindre effektiv kode hvis den ikke brukes nøye.

Nylig, med forbedringer i objektorientert programmering i PHP, foretrekker noen utviklere å bruke objekter for strukturerte data, spesielt for komplekse eller sammenkoblede datasett. Å bruke klasser kan tilby bedre innkapsling og abstraksjon, gjøre koden lettere å teste og klargjøre intensjoner. Men for enkle nøkkel-verdi lagrings- og enkle datamanipuleringsscenarioer forblir assosiative tabeller et utmerket valg på grunn av deres enkelhet og den intuitive syntaksen.
