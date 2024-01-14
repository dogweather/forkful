---
title:    "PHP: Å få dagens dato"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/php/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hvorfor
Hvorfor bry seg om å få den nåværende datoen? Det kan virke som en liten detalj, men å kjenne til den eksakte dagen kan være avgjørende for mange programmeringsoppgaver. Uten å ha tilgang til den nåværende datoen, kan det være vanskelig å lage dynamiske og nøyaktige programmer.

## Hvordan få den nåværende datoen i PHP
Det er flere måter å få tak i den nåværende datoen i PHP, men den mest vanlige og enkleste metoden er å bruke funksjonen "date()". Her er et eksempel på hvordan du kan hente ut den nåværende datoen i dagens datoformat:

```PHP
<?php
echo date("d.m.Y"); // output: 30.09.2021
?>
```

Du kan også spesifisere et annet datoformat ved å endre parametere i "date()" funksjonen. For eksempel kan du få en dato i formatet "d/m/Y" ved å skrive:

```PHP
<?php
echo date("d/m/Y"); // output: 30/09/2021
?>
```

Det er også mulig å få tilgang til andre deler av den nåværende datoen, som dag, måned eller år, ved hjelp av "date()" funksjonen. Her er noen eksempler:

```PHP
<?php
echo date("d"); // henter ut dagens dato (30)
echo date("M"); // henter ut forkortet månedsnavn (Sep)
echo date("Y"); // henter ut året (2021)
```

Det finnes også andre funksjoner for å få tak i den nåværende datoen, som "time() og "strtotime()", men "date()" funksjonen er den mest brukte og enkleste.

## Dykk dypere
Å få den nåværende datoen i PHP handler ikke bare om å få tak i dagens dag, måned og år. Det finnes også andre nyttige funksjoner og metoder for å manipulere og sjekke datoer. Du kan for eksempel legge til eller trekke fra et bestemt antall dager, måneder eller år fra en dato ved hjelp av "strtotime()" funksjonen. Her er et eksempel:

```PHP
<?php
$nyDato = date("d.m.Y", strtotime("+1 day")); // legger til en dag
echo $nyDato; // output: 01.10.2021
?>
```

Du kan også sammenligne to datoer og få ut forskjellen mellom dem ved hjelp av "strtotime()" funksjonen. Her er et eksempel:

```PHP
<?php
$dato1 = strtotime("2021-09-30"); // første dato
$dato2 = strtotime("2021-10-01"); // andre dato
$antallDager = ($dato2 - $dato1) / (60 * 60 * 24); // antall dager mellom datoene
echo $antallDager; // output: 1
?>
```

Disse er bare noen få eksempler på hva som er mulig å gjøre med datoer i PHP. Det finnes mange flere funksjoner og metoder for å håndtere datoer på en mer avansert måte.

## Se også
- [PHP.net - date() funksjonen](https://www.php.net/manual/en/function.date.php)
- [PHP.net - strtotime() funksjonen](https://www.php.net/manual/en/function.strtotime.php)
- [W3Schools - PHP Date and Time](https://www.w3schools.com/php/php_date.asp)