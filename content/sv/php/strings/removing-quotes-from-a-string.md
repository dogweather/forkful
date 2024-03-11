---
date: 2024-01-26 03:40:42.256316-07:00
description: "Att ta bort citattecken fr\xE5n en str\xE4ng i PHP betyder att ta bort\
  \ de irriterande dubbla (`\"`) eller enkla (`'`) citattecknen som kan st\xF6ka till\
  \ din\u2026"
lastmod: '2024-03-11T00:14:11.355919-06:00'
model: gpt-4-0125-preview
summary: "Att ta bort citattecken fr\xE5n en str\xE4ng i PHP betyder att ta bort de\
  \ irriterande dubbla (`\"`) eller enkla (`'`) citattecknen som kan st\xF6ka till\
  \ din\u2026"
title: "Ta bort citattecken fr\xE5n en str\xE4ng"
---

{{< edit_this_page >}}

## Vad & Varför?
Att ta bort citattecken från en sträng i PHP betyder att ta bort de irriterande dubbla (`"`) eller enkla (`'`) citattecknen som kan stöka till din kodlogik eller databasfrågor. Programmerare gör det för att städa eller sanera inmatningsdata, vilket säkerställer att strängarna kan användas eller lagras på ett säkert sätt.

## Hur:
Här är ett rakt på sak exempel som använder PHP:s inbyggda funktioner:

```php
$quotedString = "'Hej,' sa hon, \"Det är en fin dag!\"";
$unquotedString = str_replace(array("'", "\""), '', $quotedString);
echo $unquotedString; // Ger ut: Hej, sa hon, Det är en fin dag!
```

Enkelt, eller hur? Denna `str_replace()`-funktion tar en array av tecken att ta bort från strängen, inklusive både enkla och dubbla citattecken.

## Djupdykning
I PHP:s tidiga dagar var utvecklare tvungna att vara extra försiktiga med citattecken i strängar, särskilt när data skulle infogas i en databas. Felaktigt hanterade citattecken kunde leda till SQL-injektionsattacker. Då infördes magiska citattecken, en funktion som automatiskt flyktade inmatningsdata. Den blev avskaffad och slutligen borttagen eftersom den uppmuntrade till dålig kodningspraxis och säkerhetsproblem.

Nu använder vi funktioner som `str_replace()` eller regex med `preg_replace()` för mer avancerade mönster. Här är ett regex-exempel:

```php
$quotedString = "'Hej,' sa hon, \"Det är en fin dag!\"";
$unquotedString = preg_replace('/[\'"]/', '', $quotedString);
echo $unquotedString;
```

För JSON-data kan du använda `json_encode()` med alternativ som `JSON_UNESCAPED_SLASHES | JSON_UNESCAPED_UNICODE` för att undvika extra bakstreck i dina citat.

När du implementerar, överväg gränsfall. Vad händer om din sträng är menad att ha vissa citattecken, som dialog i en berättelse eller tummått? Sammanhang spelar roll, så anpassa din citatteckensborttagning till datans avsedda användning.

## Se även
- [PHP: str_replace](https://www.php.net/manual/en/function.str-replace.php)
- [PHP: preg_replace](https://www.php.net/manual/en/function.preg-replace.php)
- [PHP: json_encode](https://www.php.net/manual/en/function.json-encode.php)
- [OWASP: SQL Injection Prevention](https://owasp.org/www-community/attacks/SQL_Injection)
