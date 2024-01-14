---
title:                "PHP: Hitta längden på en sträng"
simple_title:         "Hitta längden på en sträng"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Varför
Att hitta längden på en sträng är en vanlig uppgift inom programmering och det är en grundläggande kunskap att ha inom PHP. Det kan vara användbart när man till exempel behöver validera användarinput eller manipulera textsträngar på ett effektivt sätt.

## Hur man gör det
För att hitta längden på en sträng i PHP, kan man använda funktionen `strlen()`. Detta är en inbyggd funktion som tar en sträng som argument och returnerar antalet tecken i strängen. Nedan finns ett enkelt exempel på hur man använder funktionen och dess output.

```PHP
<?php
$str = 'Hej, världen!';
echo strlen($str); // output: 13
?>
```

Som man kan se i exemplet ovan returneras värdet 13 då det finns 13 tecken i strängen "Hej, världen!". Detta gäller oavsett om det handlar om bokstäver, siffror eller specialtecken.

## Djupdykning
Det finns vissa saker att tänka på när man arbetar med längden på en sträng i PHP. Till exempel är funktionen `strlen()` beroende av teckenkodningen UTF-8, vilket innebär att den räknar både en- och flerbytestecken som ett tecken. Detta kan påverka längden på en sträng och det är därför viktigt att vara medveten om detta när man arbetar med olika teckenkodningar.

En annan viktig punkt är att funktionen `strlen()` enbart räknar antalet tecken och inte hänsyn till eventuella whitespace, som till exempel mellanslag och radbrytningar. Om man vill räkna även dessa tecken kan man använda funktionen `mb_strlen()`, som tar hänsyn till både teckenkodning och whitespace.

I vissa fall kan man även behöva manipulera strängen innan man använder funktionerna `strlen()` eller `mb_strlen()`. Till exempel kan ett mellanslag i början eller slutet av en sträng påverka längden. Därför kan det vara bra att använda funktionen `trim()` för att ta bort eventuell whitespace innan längden på strängen räknas ut.

## Se även
Här är några länkar till andra resurser som kan vara användbara för att förstå längden på en sträng i PHP:

- [PHP Manual: strlen()](https://www.php.net/manual/en/function.strlen.php)
- [PHP Manual: mb_strlen()](https://www.php.net/manual/en/function.mb-strlen.php)
- [PHP Manual: trim()](https://www.php.net/manual/en/function.trim.php)