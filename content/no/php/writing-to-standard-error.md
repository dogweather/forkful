---
title:                "PHP: Skriving til standardfeil"
programming_language: "PHP"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Hvorfor

Å skrive til standardfeil er en viktig del av PHP-programmering. Det lar utviklere enkelt spore feil og debugging-prosessen. I denne bloggposten vil vi dykke inn i hvordan man kan bruke denne funksjonen og hvorfor den er så nyttig.

## Hvordan

For å skrive til standardfeil i PHP kan man bruke funksjonen `fwrite()` sammen med `STDERR` som filresurs. Her er et eksempel:

```PHP
/* Åpner stderr som en fil for skriving */
$stderr = fopen('php://stderr', 'w');

/* Skriver en melding til standardfeil */
fwrite($stderr, 'Dette er en feilmelding som blir skrevet til standardfeil.');

/* Lukker filresursen */
fclose($stderr);
```

Når koden blir kjørt, vil meldingen bli skrevet til standardfeil og vises i utgangskonsollen. Dette er nyttig når man ønsker å identifisere og fikse feil i koden.

## Dypdykk

Det finnes også andre måter å skrive til standardfeil på i PHP. En annen metode er å bruke funksjonen `error_log()`, som lar deg skrive en feilmelding til enten standardfeil eller en annen fil eller ressurs. Ved å spesifisere en filresurs som parameter, kan man logge feilmeldingen til en bestemt fil for senere analyse.

En annen måte å skrive til standardfeil på er å bruke det populære debugging-verktøyet XDebug. Dette verktøyet lar deg sette breakpoints i koden og se verdier av variabler og funksjoner mens koden kjører. XDebug har også mulighet til å skrive til standardfeil, noe som kan være svært nyttig når man driver med debugging.

## Se også

- [PHP.net - fwrite()](https://www.php.net/manual/en/function.fwrite.php)
- [PHP.net - error_log()](https://www.php.net/manual/en/function.error-log.php)
- [XDebug Documentation](https://xdebug.org/docs/)