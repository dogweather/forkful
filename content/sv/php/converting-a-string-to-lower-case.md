---
title:                "Att konvertera en sträng till gemener"
html_title:           "PHP: Att konvertera en sträng till gemener"
simple_title:         "Att konvertera en sträng till gemener"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Varför
Att kunna konvertera en sträng till gemener (lower case) är en viktig funktion inom programmering eftersom den tillåter en mer effektiv och exakt hantering av textdata. Genom att omvandla all text till samma format undviks eventuella problem med att jämföra eller matcha strängar.

## Så här gör du
Det finns flera olika sätt att konvertera en sträng till gemener i PHP, men det enklaste sättet är att använda funktionen **strtolower()**. Denna funktion tar in en sträng som argument och returnerar en kopia av strängen i gemener.

```PHP
$str = "HEJ vÄrLdEN";
echo strtolower($str);
```

Detta skulle ge följande utskrift:

```PHP
hej världen
```

En annan metod är att använda strängfunktionen **mb_strtolower()** som är specifikt utformad för att hantera specialtecken och diakritiska tecken i flerspråkiga texter. Denna funktion kräver dock att man har aktiverat multibyte-stöd i sin PHP-installation.

```PHP
$str = "Hej världen!";
echo mb_strtolower($str, 'UTF-8');
```

Detta skulle ge samma utskrift som den första metoden. Det finns även möjlighet att använda regelbundna uttryck för att konvertera en sträng till gemener, men detta är en mer avancerad metod som inte tas upp här.

## Fördjupning
Att förstå hur man konverterar en sträng till gemener är viktigt, men det är också viktigt att förstå varför man gör det på olika sätt. Funktionen **strtolower()** är snabb och enkel att använda, men om man har att göra med textdata från flera olika språk kan det vara mer effektivt att använda **mb_strtolower()** för att säkerställa att alla specialtecken behandlas korrekt.

Det är också viktigt att komma ihåg att konvertering till gemener kan påverka både jämförelser och sortering av strängar. Om man konverterar en sträng till gemener när man jämför den med en sträng i versaler kommer de inte att matcha korrekt. Detsamma gäller vid sortering, där en sträng konverterad till gemener kommer att hamna i en annan ordning än om den hade behållit sina versaler.

## Se även
- [PHP: strtolower()](https://www.php.net/manual/en/function.strtolower.php)
- [PHP: mb_strtolower()](https://www.php.net/manual/en/function.mb-strtolower.php)
- [PHP: Reguljära uttryck](https://www.php.net/manual/en/reference.pcre.pattern.syntax.php)