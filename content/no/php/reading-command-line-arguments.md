---
title:                "PHP: Lesing av kommandolinje-argumenter"
simple_title:         "Lesing av kommandolinje-argumenter"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Hvorfor
Velkommen til vår blogg om PHP-programmering! I dag skal vi snakke om hvordan man kan lese kommandolinje-argumenter i PHP. Dette er en nyttig ferdighet å ha for å kunne lage programmer som kan kjøres fra terminalen. Les videre for å lære mer!

## Hvordan gjøre det
For å lese kommandolinje-argumenter i PHP, kan man bruke funksjonen "argv" i kombinasjon med en løkke. La oss se på et eksempel:

```PHP
<?php
// Hente ut kommandolinje-argumenter
$arguments = $_SERVER['argv'];

// Gå gjennom alle argumentene
foreach ($arguments as $argument) {
    echo $argument . "\n"; // Skriver ut hvert argument på en ny linje
}
?>
```

Hvis vi kjører dette skriptet fra terminalen med følgende kommando:

```
php command_line_args.php arg1 arg2 arg3
```

Skal vi få følgende output:

```
arg1
arg2
arg3
```

Som du kan se, vil alle argumentene vi skriver etter skriptnavnet bli lagret i en array og kan deretter behandles.

## Dypdykk
Nå som vi har lært hvordan man kan lese kommandolinje-argumenter i PHP, la oss ta en dypere titt på de forskjellige mulighetene vi har til å arbeide med dem. For eksempel kan vi bruke funksjonen "count" for å sjekke antall argumenter som ble sendt inn. Vi kan også bruke if-setninger eller switch-case for å behandle forskjellige typer argumenter. Det finnes mange forskjellige måter å håndtere kommandolinje-argumenter på, så det er viktig å finne ut hva som passer best for ditt prosjekt.

## Se også
- PHP: $_SERVER - [https://www.php.net/manual/en/reserved.variables.server.php](https://www.php.net/manual/en/reserved.variables.server.php)
- PHP: argc - [https://www.php.net/manual/en/reserved.variables.server.php#88659](https://www.php.net/manual/en/reserved.variables.server.php#88659)
- PHP: getopt - [https://www.php.net/manual/en/function.getopt.php](https://www.php.net/manual/en/function.getopt.php)