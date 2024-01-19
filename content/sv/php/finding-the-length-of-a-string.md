---
title:                "Hitta längden på en sträng"
html_title:           "Arduino: Hitta längden på en sträng"
simple_title:         "Hitta längden på en sträng"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför? 

Att hitta längden på en sträng i programmering innebär att bestämma antalet tecken som din sträng innehåller. Programmerare gör detta för att manipulera data effektivt, självkolla input eller säkerställa att strängen uppfyller specifika krav.

## Hur man gör:

Använd `strlen()` funktionen i PHP för att hämta längden på en sträng.

```PHP
<?php
$min_strang = "Hej Sverige!";
echo strlen($min_strang);
?>
```
Utmatningen av koden ovan kommer att vara `13`, det totala antalet tecken inklusive mellanslag.

## Djupdykning:

1. Historisk kontext: `strlen()` är en inbyggd funktion som introducerades i PHP 4 och finns kvar i senaste versionen av PHP.

2. Alternativ: Förutom `strlen()`, kan du också använda `mb_strlen()` som är mer relevant för multibyte strängar (t.ex. UTF-8).

```PHP
<?php
$min_strang = "Hej Sverige!";
echo mb_strlen($min_strang, 'UTF-8');
?>
```

3. Implementeringsdetaljer: `strlen()` räknar varje byte som ett tecken för ASCII strängar. För multibyte strängar rekommenderas att använda `mb_strlen()`.

## Se även:

1. [PHP Manual: strlen()](https://www.php.net/manual/en/function.strlen.php)

2. [PHP Manual: mb_strlen()](https://www.php.net/manual/en/function.mb-strlen.php)

3. [StackOverflow: How to get the string length in bytes in PHP?](https://stackoverflow.com/questions/2965361/how-to-get-string-length-in-bytes-in-php)
   
4. [Geek Hideout: PHP: Determining string length](http://www.geekhideout.com/urlcode.shtml)