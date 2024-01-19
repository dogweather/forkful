---
title:                "Omvandla en sträng till gemener"
html_title:           "Arduino: Omvandla en sträng till gemener"
simple_title:         "Omvandla en sträng till gemener"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

---

## Vad och Varför?

Att konvertera en sträng till små bokstäver innebär att ändra alla stora bokstäver i en text till deras motsvarande små bokstäver. Att göra det är ofta nödvändigt när programmakare behöver jämföra två strängar, och vill undvika falska olikheter som kan uppstå endast på grund av stora/små bokstäver.

---

## Hur man gör: 

Funktionen `strtolower()` i PHP används för att konvertera en sträng till små bokstäver. 

```PHP
<?php
$text = "HeJ sWeRiGe";
$lowercase_text = strtolower($text);
echo $lowercase_text; 
?>
```

Ovanstående kod kommer att producera följande output:

```PHP
hej sverige
```

---

## Fördjupning: 

Funktionen `strtolower()` har funnits sedan PHP 4, så den har en lång historia och är väl pålitlig. Däremot, finns det en variation av denna funktion (`mb_strtolower()`) som stödjer fler teckentyper och använder aktuell locale för att bestämma hur konverteringen ska ske.

Om du behöver konvertera stora/versala bokstäver till små bokstäver inkluderande icke-latin bokstäver, bör `mb_strtolower()` användas istället:

```PHP
<?php
mb_internal_encoding('UTF-8');
$text = "Ä Ö Ü";
$lowercase_text = mb_strtolower($text);
echo $lowercase_text;
?>
```
Koden ovan kommer att producera:

```PHP
ä ö ü
```

---

## Se även:

1. [Official PHP strtolower Documentation](http://php.net/manual/en/function.strtolower.php)
2. [Official PHP mb_strtolower Documentation](https://www.php.net/manual/en/function.mb-strtolower.php)
3. [Basic handling and manipulation of strings in PHP](https://www.w3schools.com/php/php_ref_string.asp)