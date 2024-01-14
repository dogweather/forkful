---
title:    "PHP: Omvandla en sträng till små bokstäver"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/php/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Varför

I PHP-programmering är det ibland nödvändigt att konvertera en sträng till små bokstäver för att till exempel säkerställa att en sökning blir korrekt eller för att jämföra två strängar. Det är en enkel men viktig funktion som kan ha stor inverkan på programmets funktionalitet.

## Hur man gör det

För att konvertera en sträng till små bokstäver i PHP används funktionen `strtolower()`. Nedan visas ett kodexempel och dess resultat.

```PHP
$str = "Detta är En Sträng";
echo strtolower($str);
```

Resultatet blir: "detta är en sträng".

## Djupdykning

Funktionen `strtolower()` tar endast in en parameter, strängen som ska konverteras. Den returnerar en ny sträng med alla bokstäver omvandlade till små bokstäver. Detta inkluderar även specialtecken och åäö.

Det är viktigt att tänka på att denna funktion endast konverterar bokstäver i det engelska alfabetet. Om du behöver konvertera strängar med bokstäver från andra språk behövs en annan lösning.

## Se även

För mer information om strängar i PHP och andra användbara funktioner, se följande länkar:

- [PHP Manual: strtolower()](https://www.php.net/manual/en/function.strtolower.php)
- [W3Schools: PHP String Functions](https://www.w3schools.com/php/php_ref_string.asp)
- [PHP String Functions Cheatsheet](https://github.com/Codecademy/cheatsheets/blob/master/php_string_functions/cheatsheet.md) (Engelska)