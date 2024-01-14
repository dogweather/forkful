---
title:    "PHP: Lesing av kommandolinjeargumenter"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/php/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Hvorfor 
Hvorfor lese kommandolinje-argumenter i PHP? Det er viktig for å kunne interagere med brukere og skape dynamiske programmer.

# Hvordan Du Gjør Det
For å lese kommandolinje-argumenter i PHP, må du bruke `$_SERVER['argv']` funksjonen. Denne funksjonen returnerer en array av alle argumentene gitt ved kjøring av PHP-skriptet. Her er et eksempel på en enkel måte å skrive ut argumentene og deres tilhørende indekser:

```PHP
<?php
// Les argumentene og skriv dem ut
foreach ($_SERVER['argv'] as $index => $argument) {
    echo "Argument " . $index . ": " . $argument . "\n";
}
```

Gitt at du kjører PHP-skriptet fra kommandolinjen med følgende argumenter:

```sh
php minprogram.php arg1 arg2 arg3
```

Vil outputen være:

```
Argument 0: minprogram.php
Argument 1: arg1
Argument 2: arg2
Argument 3: arg3
```

Dette viser hvordan du kan få tilgang til og bruke argumentene i PHP-skriptet ditt.

# Dypdykk

I tillegg til å kunne få tilgang til argumentene, kan du også bruke `$_SERVER['argc']` funksjonen for å få antall argumenter som ble gitt ved kjøring av skriptet. Dette kan være nyttig hvis du ønsker å kjøre ulik logikk basert på antall argumenter. Her er et eksempel på hvordan du kan gjøre det:

```PHP
<?php
// Sjekk antall argumenter
if ($_SERVER['argc'] == 3) {
    // Hvis det er tre argumenter kjører vi denne logikken
    echo "Det er 3 argumenter gitt.";
} else {
    // Hvis ikke kjører vi denne
    echo "Det er ikke 3 argumenter gitt.";
}
```

# Se Også
- [PHP.net - $_SERVER['argv']](https://www.php.net/manual/en/reserved.variables.server.php)
- [PHP.net - $_SERVER['argc']](https://www.php.net/manual/en/reserved.variables.server.php)
- [PHP: Getopt - Bruke lange argumenter](https://secure.php.net/manual/en/function.getopt.php) (Engelsk)