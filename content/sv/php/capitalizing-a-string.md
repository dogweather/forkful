---
title:    "PHP: Att Göra En Sträng Med Stora Bokstäver"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Varför
Ibland kan det vara nödvändigt att omvandla en sträng till stora bokstäver i ett PHP-program. Det kan vara för att följa en specifik formatering för en speciell webbplats eller för att jämföra strängar med varandra.

## Hur man gör
För att omvandla en sträng till stora bokstäver i PHP kan du använda funktionen `strtoupper()`. Detta är en inbyggd funktion i PHP och är enkel att använda.

```PHP
$myString = "hej på dig";
echo strtoupper($myString);
```

Output:
```PHP
HEJ PÅ DIG
```

Du kan också använda funktionen `mb_strtoupper()` för att omvandla en sträng som innehåller icke-latinska tecken. Denna funktion använder sig av multibyte-teknik för att hantera flerspråkiga tecken.

```PHP
$myString = "hej på dig こんにちは";
echo mb_strtoupper($myString, "UTF-8");
```

Output:
```PHP
HEJ PÅ DIG こんにちは
```

## Djupdykning
Det finns två viktiga saker att tänka på när du kapitaliserar en sträng i PHP:

1. Funktionen `strtoupper()` tar endast hänsyn till bokstäver som finns i ASCII-tabellen. Detta betyder att speciella tecken och bokstäver från andra språk inte kommer att omvandlas till stora bokstäver.
2. Om du använder funktionen `mb_strtoupper()` bör du specifiera en teckenkodning (t.ex. "UTF-8") för att undvika felaktiga resultat.

## Se även
- [PHP dokumentation för `strtoupper()`](https://www.php.net/manual/en/function.strtoupper.php)
- [PHP dokumentation för `mb_strtoupper()`](https://www.php.net/manual/en/function.mb-strtoupper.php)