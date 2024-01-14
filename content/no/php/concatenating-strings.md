---
title:    "PHP: Sammenslåing av strenger"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Hvorfor

Å konkatenerere strenger, eller å kombinere flere strenger til én, er et viktig konsept i PHP-programmering. Dette gjør det mulig å dynamisk bygge setninger eller tekststrenger som inneholder variabel informasjon. Det kan være nyttig for å generere dynamisk innhold på en nettside, for eksempel å vise en personlig velkomsthilsen eller en kundes navn i en faktura.

## Hvordan

For å konkatenerere strenger i PHP, bruker vi en enkel operator kalt "dot" (.). Denne operatoren kombinerer to strenger og returnerer en ny streng som er sammensatt av begge de opprinnelige strengene.

Vi kan se et eksempel på dette i kodeblokken nedenfor hvor vi kombinerer en tekststreng med en variabel som inneholder et navn:

```PHP
<?php
$navn = "Kristine";
$hilsen = "Hei, " . $navn . "! Velkommen til nettsiden vår!";
echo $hilsen;
```

Det viktigste å merke seg her er at vi bruker punktumoperatoren (. ) for å kombinere de tre delene av strengen. Output vil være: "Hei, Kristine! Velkommen til nettsiden vår!"

Vi kan også konkatenerere flere strenger sammen ved å bruke flere punktumoperatorer, som vist i eksempelet nedenfor:

```PHP
<?php
$fornavn = "Kristine";
$etternavn = "Hansen";
echo $fornavn . " " . $etternavn;
```

Output vil være: "Kristine Hansen".

Vi kan også kombinere flere variabler ved hjelp av punktumoperatoren, ikke bare tekststrenger som i eksempelet over.

## Dypdykk

Det er viktig å merke seg at når vi konkatenerer tall og strenger, vil PHP automatisk konvertere tallene til en streng før de blir kombinert. Dette kan føre til uventede resultater hvis vi ikke er oppmerksomme. For eksempel, hvis vi prøver å konkatenerere et tall og en streng, som i eksempelet nedenfor:

```PHP
<?php
$tall = 5;
$tekst = " stjerner";
echo "Denne filmen fikk " . $tall . $tekst;
```

Output vil være: "Denne filmen fikk 5 stjerner". PHP konverterer automatisk tallet $tall til en streng og kombinerer den med resten av stringen, i stedet for å utføre et matematisk uttrykk.

Det er også viktig å være oppmerksom på at det er en forskjell mellom en enkeltkolon (:) i PHP og punktumoperatoren (. ). Enkeltkolon blir brukt til å sette av variabelnavn i en tekststreng, mens punktumoperatoren brukes til å konkatenerere strenger.

## Se også

- [Offisiell PHP-dokumentasjon om konkatenering](https://www.php.net/manual/en/language.operators.string.php)
- [Enkel guide til strenger i PHP](https://www.w3schools.com/php/php_string.asp)
- [Interaktiv tutorial for å lære å konkatenerere strenger i PHP](https://www.codecademy.com/learn/learn-php/modules/php-strings/cheatsheet)