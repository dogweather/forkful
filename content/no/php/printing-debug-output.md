---
title:                "Utskrift av feilsøkingsresultat"
html_title:           "Arduino: Utskrift av feilsøkingsresultat"
simple_title:         "Utskrift av feilsøkingsresultat"
programming_language: "PHP"
category:             "PHP"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/printing-debug-output.md"
---

{{< edit_this_page >}}

# **Utskrift av Debug Output i PHP**

## Hva & Hvorfor?
Feilsøkingsutskrift, eller "debug output", er en metode for å vise variabler eller resultater til brukeren under kjøretiden. Programvareutviklere bruker dette som et sentralt feilsøkingsverktøy for å finne og løse feil.

## Hvordan Gjøre dette:
Feilsøkingsutskriver i PHP kan gjøres med `echo`, `print`, `print_r`, `var_dump` eller `var_export`. 

Her er et eksempel ved bruk av `echo` og `var_dump`:

```PHP
<?php
$test_var = "Se meg på skjermen!";
echo $test_var; 

$test_array = array(1, "to", 3, "fire");
var_dump($test_array);
?>
```

Dette vil produsere følgende utskrift: 

```PHP
Se meg på skjermen!
array(4) {
  [0]=>
  int(1)
  [1]=>
  string(2) "to"
  [2]=>
  int(3)
  [3]=>
  string(4) "fire"
}
```

## Dypdykk
Historisk sett har PHP alltid hatt innebygde metoder for utskrift av feilsøkingsinformasjon. Alternativene til de grunnleggende innebygde funksjonene er som oftest mer utviklede biblioteker eller rammebetingelser, som Xdebug og Kint. 

`echo` og `print` er de enkleste, og viser rett og slett verdien som en streng. `print_r` er en funksjon som gir en lesbar utskrift av en variabel i en måte som er lesbar for mennesker. `var_dump` og `var_export` vil også inkludere typene og verdien(e), men forskjellen er at `var_export` returnerer en gyldig PHP-kode.

## Se Også
For dypere kunnskap om feilsøking i PHP, sjekk ut følgende ressurser:
- PHP.net's debugging oversikt: [https://www.php.net/manual/en/debugger.php](https://www.php.net/manual/en/debugger.php)
- Xdebug, en PHP-utvidelse for feilsøking: [https://xdebug.org/](https://xdebug.org/)
- Kint, en moderne og kraftig PHP-feilsøkingsverktøy: [https://kint-php.github.io/kint/](https://kint-php.github.io/kint/)