---
title:    "PHP: Skriving til standardfeil"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Hvorfor
I denne bloggposten skal vi snakke om en viktig del av PHP-programmering - å skrive til standardfeil. Dette er en viktig ferdighet for alle som ønsker å bli en dyktig PHP-utvikler, så i dag skal vi se på hvorfor dette er viktig og hvordan man kan gjøre det riktig.

## Hvordan gjøre det
For å skrive til standardfeil i PHP, kan du bruke den innebygde funksjonen `error_log()`. Denne funksjonen tar inn en beskjed som skal skrives til standardfeil, og i tillegg kan du spesifisere hvor beskjeden skal skrives. La oss se på et enkelt eksempel:

```PHP
<?php
$error_message = "Noe gikk galt i koden!";
error_log($error_message);
```

I dette eksemplet har vi definert en beskjed og brukt funksjonen `error_log()` til å skrive den til standardfeil. I output vil vi se denne beskjeden sammen med annen informasjon som dato og tidspunkt for feilen. Dette er spesielt nyttig når du jobber med større prosjekter og må finne ut hvor en feil oppstår.

En annen nyttig funksjon i PHP er `ini_set()`. Med denne funksjonen kan du endre feilrapporteringsinnstillingene for PHP midlertidig. For eksempel kan du bruke denne funksjonen til å deaktivere PHP-feilrapportering mens du tester eller utvikler en del av koden din. Etter at du er ferdig med testingen, bør du huske å sette innstillingene tilbake til standarden.

## Dykk dypere
Selv om det er viktig å kunne skrive til standardfeil i PHP, er det også viktig å forstå hvordan feilbehandling fungerer i språket. PHP har ulike typer feil, som advarsler, notiser og fatalfeil. Disse behandles forskjellig av koden og kan ha ulike konsekvenser for programmet ditt.

En annen viktig aspekt er feillogging og hvordan beskjeder blir behandlet. Det er viktig å lagre feilmeldinger på en pålitelig måte, spesielt når flere personer jobber på samme kodebase. Dette gjør det mye enklere å finne og rette feil når de oppstår.

## Se også
- [PHP manual for error_log()](https://www.php.net/manual/en/function.error-log.php)
- [PHP manual for ini_set()](https://www.php.net/manual/en/function.ini-set.php)
- [PHP manual for error handling](https://www.php.net/manual/en/language.errors.php)