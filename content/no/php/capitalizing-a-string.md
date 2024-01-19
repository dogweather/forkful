---
title:                "Gjøre en streng stor"
html_title:           "PHP: Gjøre en streng stor"
simple_title:         "Gjøre en streng stor"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/capitalizing-a-string.md"
---

{{< edit_this_page >}}

---
## Hva & Hvorfor?
Å kapitalisere en string betyr å gjøre første bokstav i en setning eller en ord til en stor bokstav. Programmers gjør dette for å forbedre lesbarheten og presentasjonen av data, for eksempel titler eller navn.

## Hvordan utføre:
I PHP kan vi bruke den innebygde `ucfirst()` funksjonen for å kapitalisere en streng. Her er et eksempel:

```PHP
<?php
    $streng = "heisann, verden!";
    $kapitalisert_streng = ucfirst($streng);
    echo $kapitalisert_streng;
?>
```
Dette vil returnere:
```PHP
"Heisann, verden!"
```

## Dyp Dykk
Historisk sett, har kapitalisering av strenger blitt brukt i bøker og dokumenter lenge før datamaskinene ble oppfunnet. I forbindelse med programmering, lar det oss håndtere data på en mer leservennlig måte.

Alternativt til `ucfirst()`, kan vi også bruke `mb_convert_case()` i PHP når vi trenger å håndtere multibyte strenger (for eksempel, strenger med spesielle tegn eller emojis).

Her er et eksempel:
```PHP
<?php
   $streng = "hei𝄞san, verden!"; // her bruker vi et musikalsk symbol som ikke er en del av det normale ASCII-settet
   $kapitalisert_streng = mb_convert_case($streng, MB_CASE_TITLE, "UTF-8");
   echo $kapitalisert_streng;
?>
```
Dette vil returnere:
```PHP
"Hei𝄞San, Verden!"
```
Vær oppmerksom på at `mb_convert_case()` ikke bare kapitaliserte begynnelsen av strengen, men også etter hvert symbol som ikke er en del av det normale ASCII-settet.

## Se Også
- PHP offisielle dokumentasjon på `ucfirst()` : http://php.net/manual/en/function.ucfirst.php
- PHP offisielle dokumentasjon på `mb_convert_case()` : http://php.net/manual/en/function.mb-convert-case.php
- W3Schools' tutorial på PHP String Functions: http://www.w3schools.com/php/php_ref_string.asp