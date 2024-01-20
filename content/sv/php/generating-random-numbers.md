---
title:                "Generera slumpmässiga nummer"
html_title:           "Arduino: Generera slumpmässiga nummer"
simple_title:         "Generera slumpmässiga nummer"
programming_language: "PHP"
category:             "PHP"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att generera slumptal är processen att producera siffror på ett tillfälligt ordnat sätt för att simulera slumpmässighet. Programmerare gör detta för att injicera osäkerhet i sina koder, ofta för att underlätta tester och simuleringar eller skapa slumpmässiga spelresultat.

## Så här gör man:
Att generera ett slumptal i PHP är relativt rakt på sak. Följande kodskärv visar hur man kan skapa ett slumptal mellan 1 och 10:

```PHP
<?php
$randomNum = rand(1, 10);
echo $randomNum;
?>
```
Output för ovanstående kod kan vara något nummer mellan 1 och 10, eftersom det är de gränser vi satte med `rand(1, 10)`.

Exempelvis,
```PHP
7
```
 
## Fördjupning
Generering av slumptal har en lång historia i programmering, och PHP är inget undantag. Från ett histormässigt perspektiv, var `rand()` det ursprungliga sättet att skapa slumptal i PHP. Dock, för mer kryptografiskt säkra ändamål, introducerades PHP 7-funktionen `random_int()`.

Alternativt till `rand()`, kan du använda `mt_rand()`, vilket är snabbare och producerar bättre slumptal men kanske inte är så kryptografiskt säkert som `random_int()`

För att förstå implementeringsdetaljer i PHP, är det viktigt att förstå att dessa funktioner används för att skapa pseudo-slumptal. Det innebär att de producerar nummer som ser slumpmässiga ut, men Genereras av en deterministisk process.

## Se också
För mer information om slumptalsgenerering i PHP och relaterade ämnen, besök följande länkar:

1. PHP Manual på [rand()](https://www.php.net/manual/en/function.rand.php) och [random_int()](https://www.php.net/manual/en/function.random-int.php)
2. PHP Manual på [mt_rand()](https://www.php.net/manual/en/function.mt-rand.php)
3. En artikel om [cryptographically secure pseudo-random number](https://paragonie.com/blog/2015/07/how-safely-generate-random-strings-and-integers-in-php)