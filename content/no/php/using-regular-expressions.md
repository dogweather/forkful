---
title:                "Bruk av regulære uttrykk"
date:                  2024-01-19
html_title:           "Bash: Bruk av regulære uttrykk"
simple_title:         "Bruk av regulære uttrykk"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Regular expressions (regex) lar deg søke og manipulere tekst basert på mønstre. Programmerere bruker det for å effektivisere tekstbehandling, som datavalidering og -rensing.

## Hvordan gjøre det:
```PHP
<?php
$tekst = "Finn meg på email@example.com for mer info.";
$regex_pattern = "/[a-zA-Z0-9._%-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,4}/";

if (preg_match($regex_pattern, $tekst, $matcher)) {
    echo "Funnet e-post: " . $matcher[0] . "\n"; // Output: Funnet e-post: email@example.com
} else {
    echo "Ingen e-post funnet.";
}
?>
```

## Dypdykk
Regular expressions har røtter tilbake til teoretisk informatikk fra 1950-tallet. Alternativer inkluderer tekstfunksjoner som `strpos()` for direkte søk, eller `str_replace()` for enkel erstatning, men de er mindre dynamiske. PHP implementerer regex via `PCRE` (Perl Compatible Regular Expressions), som støtter et bredt spekter av mønstre og operasjoner for komplekse søk.

## Se også
- PHP Manual on PCRE: https://www.php.net/manual/en/book.pcre.php
- Regex101 for å teste regex online: https://regex101.com/
- PHP preg_match documentation: https://www.php.net/manual/en/function.preg-match.php
