---
title:                "Hämta aktuellt datum"
html_title:           "Arduino: Hämta aktuellt datum"
simple_title:         "Hämta aktuellt datum"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Vad och varför?
Att få det aktuella datumet i PHP innebär att ta reda på vilket datum det är just nu. Detta används ofta av programmerare för att spåra händelser, logga data och hantera tidsbegränsade funktioner.

## Så här gör du:
PHP erbjuder oss en inbyggd funktion, `date()`, för att få det aktuella datumet. Kolla in följande exempel:

```PHP
<?php
    echo date("Y-m-d");
?>
```

Ovanstående kod kommer att skriva ut datumet i formatet YYYY-MM-DD. Till exempel: `2021-12-01`. 

## Djupdykning
PHP:s `date()`-funktion har lång historia. Den har varit en del av PHP-kärnan sedan version 4 släpptes 2000. Det finns alternativ till `date()`, som `DateTime`-objektet introducerat i PHP 5.2. Denna klass ger fler metoder och mer flexibilitet vid hantering av datum och tid.

Vad gäller implementationen returnerar `date()`-funktionen en sträng som representerar det aktuella datumet, baserat på serverns inställningar för tidszon och systemtid. Var försiktig när du använder `date()` i distribuerade system, serverns tid kanske inte alltid är vad du förväntar dig.

## Se även
För mer information om `date()` och datumhantering i PHP, se följande resurser:

- PHP officiella dokumentation för datumfunktioner: [https://www.php.net/manual/en/book.datetime.php](https://www.php.net/manual/en/book.datetime.php)
- PHP DateTime-klass: [https://www.php.net/manual/en/class.datetime.php](https://www.php.net/manual/en/class.datetime.php)
- Guide till PHP:s datum- och tidsfunktioner: [https://www.w3schools.com/php/php_date.asp](https://www.w3schools.com/php/php_date.asp)