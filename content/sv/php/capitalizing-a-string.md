---
title:                "Att omvandla en sträng till versaler"
html_title:           "PHP: Att omvandla en sträng till versaler"
simple_title:         "Att omvandla en sträng till versaler"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att kunna kapitalisera en sträng kan vara användbart vid formatering av text, till exempel för att skapa rubriker eller betona vissa ord. Det är också ett grundläggande koncept inom programmering och kan hjälpa till att förbättra kodens läsbarhet.

## Hur man gör det

För att kapitalisera en sträng i PHP kan du använda funktionen `strtoupper()`. Här är en kod som visar hur man använder den:

```
<?php
$str = "hej på dig";
echo strtoupper($str);
?>
```

Detta skulle producera outputen "HEJ PÅ DIG". Det är viktigt att notera att `strtoupper()` endast fungerar på bokstäver och att symboler och siffror inte påverkas av funktionen.

Du kan också använda funktionen `ucwords()` om du vill kapitalisera varje ord i en sträng istället för bara den första bokstaven. Här är ett exempel:

```
<?php
$str = "denna sträng kommer att kapitaliseras";
echo ucwords($str);
?>
```

Outputen skulle vara "Denna Sträng Kommer Att Kapitaliseras".

## Djupdykning

En annan funktion som kan vara användbar för att kapitalisera strängar är `ucfirst()`. Denna funktion kapitaliserar endast den första bokstaven i en sträng. Det kan vara användbart om du bara vill betona en del av en sträng istället för att kapitalisera hela den. Här är ett exempel på hur man kan använda `ucfirst()`:

```
<?php
$str = "bara den här första bokstaven ska kapitaliseras.";
echo ucfirst($str);
?>
```

Outputen skulle vara "Bara den här första bokstaven ska kapitaliseras."

Det finns också möjlighet att använda `strtolower()` för att göra en sträng helt omvänd, det vill säga alla bokstäver blir små. Detta kan vara användbart om du vill göra en sträng helt enkel att bearbeta. Till exempel, om du vill matcha en sökfras men vill undvika skillnader i stora och små bokstäver. Här är ett exempel på hur man kan använda `strtolower()`:

```
<?php
$str = "DEN HÄR STRÄNGEN BLIR SMÅ BOKSTÄVER.";
echo strtolower($str);
?>
```

Outputen skulle vara "den här strängen blir små bokstäver."

## Se även

- PHP-dokumentation för `strtoupper()`: https://www.php.net/manual/en/function.strtoupper.php
- PHP-dokumentation för `ucwords()`: https://www.php.net/manual/en/function.ucwords.php
- PHP-dokumentation för `ucfirst()`: https://www.php.net/manual/en/function.ucfirst.php
- PHP-dokumentation för `strtolower()`: https://www.php.net/manual/en/function.strtolower.php