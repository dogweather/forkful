---
title:                "PHP: Användning av reguljära uttryck"
programming_language: "PHP"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Varför
Reguljära uttryck är ett kraftfullt verktyg i PHP-programmering som gör det möjligt att söka, matcha och ersätta textmönster. Genom att använda reguljära uttryck kan du effektivt filtrera och manipulera data på ett flexibelt sätt. Det är ett oumbärligt verktyg för att hantera text och strängar i dina PHP-program.

## Hur man
```PHP
$str = "Det finns 10 fåglar i en trädgård";
if (preg_match("/\d+/", $str, $matches)) {
    echo "Antal fåglar: " . $matches[0]; //Output: 10
} else {
    echo "Ingen matchning hittades.";
}
```

Du kan använda reguljära uttryck i PHP genom att använda funktionerna `preg_match()` eller `preg_replace()`. Det första argumentet är själva uttrycket som du vill matcha mot, det andra argumentet är strängen du vill söka igenom och det tredje argumentet är en variabel där matchningen eller ersättningsresultatet sparas. Genom att använda speciella tecken och kvantiteter kan du skapa mer avancerade och precisa mönster för matchning. Det finns olika online resurser och verktyg som kan hjälpa dig att bygga och testa dina reguljära uttryck.

## Djupdykning
Reguljära uttryck kan vara komplexa men är mycket kraftfulla. De gör det möjligt att söka och hantera text på ett effektivt sätt jämfört med vanliga strängmetoder. Regelbundna uttryck är också ett viktigt koncept inom datahantering och datautvinning, och kan användas för att skapa filtrerings- och sökkriterier i databaser. Det är väl värt att spendera tid på att lära sig och praktisera denna teknik för att bli en mer effektiv programmerare.

## Se även
- [Officiell PHP-dokumentation för reguljära uttryck](https://www.php.net/manual/en/book.pcre.php)
- [RegExr - Online reguljära uttryck verktyg](https://regexr.com/)
- [Reguljära uttryck 101 - Lär dig, bygg och testa reguljära uttryck](https://regex101.com/)