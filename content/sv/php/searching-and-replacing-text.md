---
title:                "Söka och ersätta text"
html_title:           "PHP: Söka och ersätta text"
simple_title:         "Söka och ersätta text"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Sökning och ersättning av text är en vanlig uppgift för programmerare, där man söker efter specifika delar av text och ersätter dem med annan text. Det kan vara användbart för att göra stora ändringar i en textfil utan att behöva göra ändringar manuellt.

## Hur man gör:
En enkel metod för att söka och ersätta text är att använda PHP-funktionen "str_replace()". Den här funktionen tar tre parametrar - det sökta ordet, den text som ska ersätta det, och den sträng där sökningen ska utföras.

```PHP
$text = "Det här är en text som behöver ändras";
$ersattning = str_replace("behöver", "kan", $text);
echo $ersattning;
```
Output: "Det här är en text som kan ändras"

Man kan också söka efter flera ord eller fraser samtidigt genom att använda arrays som parametrar. Om man vill ersätta både "behöver" och "ändras" i exemplet ovan kan man skriva det så här:

```PHP
$ersattning = str_replace(["behöver", "ändras"], ["kan", "börja"], $text);
echo $ersattning;
```
Output: "Det här är en text som kan börja"

## Djupdykning:
Sökning och ersättning av text är inte bara användbart för enstaka ord eller fraser, utan kan också användas för att göra avancerade reguljära uttryck (regex) på en sträng. PHP har inbyggda funktioner som preg_replace() för att hantera regex i sökningar och ersättningar.

En annan metod för att söka och ersätta text är med hjälp av regular expressions libraries som PCRE eller GREP, som ger mer flexibilitet och funktionalitet än de inbyggda PHP-funktionerna.

## Se även:
- [PHP "str_replace()" Dokumentation](https://www.php.net/manual/en/function.str-replace.php)
- [PhpStorm editor med inbyggda regular expression-funktioner](https://www.jetbrains.com/help/phpstorm/regular-expression-syntax-reference.html)