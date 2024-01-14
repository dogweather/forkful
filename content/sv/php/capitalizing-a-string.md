---
title:                "PHP: Att Kapitalisera en Sträng"
simple_title:         "Att Kapitalisera en Sträng"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att kapitalisera en sträng är en viktig funktion inom PHP-programmering för att vi vill kunna ändra textsträngar på ett effektivt sätt. Det kan vara användbart för att formatera text, skapa rubriker eller för att matcha en viss standard i en databas.

## Hur man gör

För att kapitalisera en sträng i PHP, kan du använda funktionen "ucwords()". Detta kommer att förvandla den första bokstaven i varje ord i en sträng till en stor bokstav.

```PHP
$str = "hej, jag heter john smith";
echo ucwords($str);
```
Output: Hej, Jag Heter John Smith

För att helt kapitalisera en sträng, kan du använda funktionen "strtoupper()". Detta kommer att förvandla alla bokstäver i en sträng till stora bokstäver.

```PHP
$str = "välkommen till min blogg";
echo strtoupper($str);
```
Output: VÄLKOMMEN TILL MIN BLOGG

Det finns också möjlighet att bara kapitalisera första bokstaven i en sträng med hjälp av "ucfirst()". Detta kan vara användbart om du vill skapa rubriker eller förnamn.

```PHP
$str = "jennifer aniston";
echo ucfirst($str);
```
Output: Jennifer aniston

## Deep Dive

En annan viktig funktion inom PHP är "mb_convert_case()". Med denna funktion kan du ange vilken typ av kapitalisering du vill ha för en sträng, som "MB_CASE_TITLE" för att göra alla första bokstäver i varje ord stora.

```PHP
$str = "hur annonserar jag min blogg?";
echo mb_convert_case($str, MB_CASE_TITLE, "UTF-8");
```
Output: Hur Annonserar Jag Min Blogg?

Det är också viktigt att komma ihåg att språket påverkar hur en sträng kapitaliseras. Om du jobbar inom ett annat språk än engelska, kan du använda "setlocale()" för att ställa in rätt språk och därmed få önskad kapitalisering.

## Se även

- [PHP Manual: ucwords()](https://www.php.net/manual/en/function.ucwords.php)
- [PHP Manual: strtoupper()](https://www.php.net/manual/en/function.strtoupper.php)
- [PHP Manual: ucfirst()](https://www.php.net/manual/en/function.ucfirst.php)
- [PHP Manual: mb_convert_case()](https://www.php.net/manual/en/function.mb-convert-case.php)
- [PHP Manual: setlocale()](https://www.php.net/manual/en/function.setlocale.php)