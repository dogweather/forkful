---
title:                "Hitta längden på en sträng"
date:                  2024-01-20T17:48:09.496604-07:00
model:                 gpt-4-1106-preview
simple_title:         "Hitta längden på en sträng"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att hitta längden på en sträng innebär att räkna antalet tecken den innehåller. Programmerare gör detta för att validera indata, manipulera text eller bara för att jämföra storlek.

## Så här gör du:
Här är hur du använder `strlen()` för att få längden på en sträng. Exemplet är rakt på sak:

```php
<?php
$text = "Hej, Sverige!";
$length = strlen($text);
echo $length; // Skriver ut 13
?>
```

`strlen()` är en inbyggd PHP-funktion som ger oss antalet tecken i en sträng. Enkelt och effektivt.

## Djupdykning:
För länge sedan, när PHP var ungt, användes `strlen()` ganska okomplicerat. Men när PHP började stödja fler teckenkodningar, blev saker lite krångligare. Från och med PHP 5.2.0 kan vi använda `mb_strlen()` när vi har att göra med multibyte-teckenkodningar som UTF-8, där ett tecken faktiskt kan vara mer än ett byte. Ett "ä" i UTF-8 räknas dock fortfarande som ett tecken, även om det tar upp mer än ett byte.

Alternativen till `strlen()` inkluderar `mb_strlen()` som nämndes ovan, samt `grapheme_strlen()` om du behöver få längden på en sträng baserat på Unicode grafemkluster.

Implementationen av `strlen()` är rakt på sak och använder C-funktionen `strlen`. Det är därför den är snabb, men det betyder också att den inte tar hänsyn till multibyte-teckenkodningar.

## Se även:
- PHPs officiella dokumentation om [`strlen()`](https://www.php.net/manual/en/function.strlen.php)
- För en djupdykning i multibyte-strängar, se [`mb_strlen()`](https://www.php.net/manual/en/function.mb-strlen.php)
- Unicode och grafemkluster: [`grapheme_strlen()`](https://www.php.net/manual/en/function.grapheme-strlen.php)
- W3Schools sida om PHP string-funktioner: [PHP String Functions](https://www.w3schools.com/php/php_ref_string.asp)
