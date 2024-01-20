---
title:                "Gör om en sträng till versaler"
html_title:           "PHP: Gör om en sträng till versaler"
simple_title:         "Gör om en sträng till versaler"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Att Kapitalisera En Sträng i PHP: Allt du Behöver Veta

## Vad & Varför?

Kapitalisering av en sträng innebär att göra första bokstaven i denna sträng till stor. Programmerare gör detta för att förbättra textens läsbarhet och för att möta säkerhetskriterier.

## Hur gör man:

PHP erbjuder en inbyggd funktion - ucfirst(). Denna funktion gör första bokstaven i strängen kapitaliserad.

```PHP
<?php
$text = 'hej världen';
$capitalizedText = ucfirst($text);
echo $capitalizedText;
?>
```

Detta skulle skriva ut:

```PHP
Hej världen
```

## Djupdykning

Kapitalisering av strängar har varit vanligt i programmering för att förbättra läsbarheten. Historiskt sett var det nödvändig i tidiga programmeringsspråk som COBOL där all kod var i versaler.

Det finns andra funktioner som liknar ucfirst i PHP. En av dem är strtoupper, som omvandlar alla tecken i en sträng till stor.

```PHP
<?php
$text = 'hej världen';
$upperText = strtoupper($text);
echo $upperText;
?>
```

Det skulle skriva ut:

```PHP
HEJ VÄRLDEN
```
Implementeringen av ucfirst() i PHP är ganska enkel, den lokaliserar det första alfabetiska tecknet i strängen, förvandlar det till ett stort bokstav och returnerar sedan den ändrade strängen.

## Se även

För ytterligare detaljer om strängmanipulation i PHP, se följande källor:

- [PHP.net's sträng funktioner](https://www.php.net/manual/en/ref.strings.php)
- [W3Schools PHP Strängreferens](https://www.w3schools.com/php/php_ref_string.asp)
- [GeeksforGeeks artikel om PHP strängfunktioner](https://www.geeksforgeeks.org/string-functions-in-php/)