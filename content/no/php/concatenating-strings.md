---
title:                "Sammenslåing av strenger"
html_title:           "Arduino: Sammenslåing av strenger"
simple_title:         "Sammenslåing av strenger"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/concatenating-strings.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Strengkonkatinering i PHP er prosessen med å sette sammen to eller flere strenger. Programmerere bruker dette for å dynamisk skape nye strenger ut fra eksisterende data.

## Slik gjør du det:

Her er et grunnleggende eksempel på hvordan du kan konkatere strenger i PHP:

```PHP
$hello = "Hei";
$world = "verden";
$complete_sentence = $hello . " " . $world;

echo $complete_sentence; // Outputs: Hei verden
```

En alternativ metode er å bruke `.= operator` for å tillegge en streng til en eksisterende streng:

```PHP
$hello = "Hei";
$hello .= " verden";

echo $hello; // Outputs: Hei verden
```

## Dypdykk:

Historisk, før PHP 5.3, brukte utviklere ofte funksjonene `sprintf` og `printf` for å utføre strengkonkatenering. Men `. og .=` operatørene ga en enklere og mer intuitiv løsning, noe som gjorde dem til standarden.

Selv om `. og .= operatørene` er de mest vanlige og intuitive måtene å konkatere strenger i PHP, er det alternative metoder. En av disse er å bruke `sprintf` funksjonen, som fortsatt er mye brukt i koden som krever formatering av strengen:

```PHP
$hello = "Hei";
$world = "verden";
$complete_sentence = sprintf("%s %s", $hello, $world);

echo $complete_sentence; // Outputs: Hei verden
```

Når det gjelder ytelse mellom `. og .= operatørene` og `sprintf`, er forskjellene minimale og vil bare være merkbare i applikasjoner med veldig høy last.

## Se Også:

For flere detaljer om strengkonkatinering i PHP, sjekk ut disse nyttige lenkene:
- PHP Official Documentation: [String concatenation](https://www.php.net/manual/en/language.operators.string.php)
- Stack Overflow: [Concatenate or Directly Insert Variables into Strings?](https://stackoverflow.com/questions/41367012/php-concatenate-or-directly-insert-variables-into-string)
- W3School: [PHP String Functions](https://www.w3schools.com/php/php_ref_string.asp)