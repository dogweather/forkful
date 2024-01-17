---
title:                "Å finne lengden på en streng"
html_title:           "PHP: Å finne lengden på en streng"
simple_title:         "Å finne lengden på en streng"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å finne lengden til en streng er en vanlig oppgave i programmering, og det refererer til å bestemme antall tegn eller bytes i en tekststreng. Dette gjøres ofte for å kunne behandle strenger på riktig måte, for eksempel å begrense brukerinndata eller formatere tekst på en bestemt måte.

## Slik gjør du:
Her er et eksempel på hvordan du finner lengden av en streng i PHP:
```PHP
<?php
$string = "Dette er en streng.";
echo strlen($string); // output: 21
?>
```

Slik fungerer eksempelet:
- Først definerer vi en variabel som inneholder en tekststreng.
- Deretter bruker vi funksjonen `strlen()` til å finne lengden av strengen.
- Til slutt skriver vi ut resultatet ved å bruke `echo`.

Du kan også bruke `mb_strlen()` i stedet hvis du trenger å håndtere flerspråklige strenger som inneholder andre språk enn det som benytter det latinske alfabetet.

## Dypdykk:
Før i tiden, da datamaskiner brukte begrensede mengder data, ble lengden til en streng ofte brukt til å optimalisere plass og ytelse. I dag brukes det mest som et praktisk mål for å behandle tekststrenger på en riktig måte.

Alternativer til å bruke `strlen()` inkluderer å bruke en løkke til å telle antall tegn eller å bruke funksjonen `count_chars()` for å telle unike tegn. Det finnes også lignende funksjoner i andre programmeringsspråk som Python ( `len()` ) og Java ( `length()` ).

Når det kommer til implementering, er `strlen()` en innebygd funksjon i PHP, noe som betyr at du ikke trenger å importere noen ekstra biblioteker for å bruke den.

## Se også:
- [PHP.net - Get String Length](https://www.php.net/manual/en/function.strlen.php)
- [W3Schools - PHP String Functions](https://www.w3schools.com/php/php_ref_string.asp)