---
title:                "PHP: Omvandling av en sträng till gemener"
simple_title:         "Omvandling av en sträng till gemener"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Varför
Att konvertera en sträng till små bokstäver är en vanlig uppgift för PHP utvecklare. Genom att göra en sträng till små bokstäver kan du effektivt jämföra eller manipulera texten utan att oroa dig för skillnader i bokstavsstorlek.

## Hur man gör
Att konvertera en sträng till små bokstäver i PHP är en enkel process. Du kan använda funktionen `strtolower()` som tar in en sträng som argument och returnerar strängen med alla bokstäver omvandlade till små. Låt oss titta på ett exempel:

```PHP
$str = "DET HÄR ÄR EN STRÄNG MED STORA BOKSTÄVER";
echo strtolower($str);

// output: det här är en sträng med stora bokstäver
```
Som du kan se i exemplet konverterar `strtolower()` funktionen alla bokstäverna i strängen till små. Detta är en användbar funktion när du vill jämföra två strängar som kan ha olika bokstavsstorlekar.

## Djupdykning
Vad händer om du har en sträng som innehåller tecken som inte är bokstäver, som siffror eller specialtecken? I sådana fall kommer `strtolower()` funktionen inte att påverka dem och de kommer att behålla sin ursprungliga form. Detta kan vara viktigt att ha i åtanke när du manipulerar strängar.

En annan viktig aspekt att notera är att `strtolower()` funktionen endast fungerar med bokstäver i ASCII-teckenuppsättningen. Om du använder icke-engelska bokstäver eller symboler från andra språk kan resultatet vara annorlunda. I sådana fall kan du använda funktionen `mb_strtolower()` som stöder flera språk.

## Se även
- [PHP: strtolower() function](https://www.php.net/manual/en/function.strtolower.php)
- [PHP: mb_strtolower() function](https://www.php.net/manual/en/function.mb-strtolower.php)