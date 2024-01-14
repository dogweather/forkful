---
title:                "PHP: Omvandla en sträng till gemener"
programming_language: "PHP"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Varför

Det kan finnas flera anledningar till att man vill konvertera en sträng till små bokstäver. Det kan till exempel vara för att man vill ha en enhetlig formatering av texten, för ett mer enhetligt utseende eller för att undvika förvirring vid jämförelse av textsträngar.

## Hur man gör

För att konvertera en sträng till små bokstäver kan man använda PHP funktionen strtolower(). Detta kan enkelt göras genom att ge strängen som man vill konvertera som ett argument till funktionen. Nedan ser du ett exempel på hur man gör:

```PHP
$text = "DETTA ÄR EN TEXT";
echo strtolower($text);
```
Output:
`detta är en text`

Det är viktigt att notera att detta endast fungerar för bokstäver i det engelska alfabetet, så användningen av andra tecken kan leda till oönskade resultat.

## Deep Dive

Om man vill få en djupare förståelse för hur funktionen strtolower() fungerar är det bra att veta att den bygger på Unicode teckenattribut. Detta gör att den kan hantera olika språk och karaktärer på ett korrekt sätt.

Ytterligare en intressant detalj är att när man konverterar en sträng till små bokstäver, byts även eventuella accenter ut till motsvarande vanliga bokstäver. Detta kan vara bra att veta om man behöver hantera flerspråkiga texter.

## Se också

- Denna artikel är skriven av Muhammad Suleman, som ger en bra överblick över hur man kan konvertera en sträng till små bokstäver i PHP: [https://www.cloudways.com/blog/php-string-lowercase/](https://www.cloudways.com/blog/php-string-lowercase/)
- PHPs officiella dokumentation kring funktionen strtolower(): [https://www.php.net/manual/en/function.strtolower.php](https://www.php.net/manual/en/function.strtolower.php)
- En guide för att hantera språk och teckenkodning i PHP: [https://www.toptal.com/php/a-utf-8-primer-for-php-and-mysql](https://www.toptal.com/php/a-utf-8-primer-for-php-and-mysql)