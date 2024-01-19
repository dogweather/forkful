---
title:                "Skriva ut felsökningsresultat"
html_title:           "Fish Shell: Skriva ut felsökningsresultat"
simple_title:         "Skriva ut felsökningsresultat"
programming_language: "PHP"
category:             "PHP"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/printing-debug-output.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att skriva ut debug-utdata är ett sätt för programmerare att övervaka och analysera programmens körtid. Detta görs för att lösa eventuella problem eller buggar och bekräfta att kodsegment fungerar korrekt.

## Hur till:

Här är ett exempel på hur du kan skriva ut debug-utdata i PHP:

```PHP
<?php
function debug($var){
    echo '<pre>';
    var_dump($var);
    echo '</pre>';
}

debug($_SERVER);
?>
```
När du kör koden ovan kommer output att se ut som följande:

```PHP
array(29) {
...
...
}
```
Med PHP kan du använda funktionen `var_dump()` för att skriva ut både datatypen och värdet. Använd `pre`-taggar för att förbättra läsbarheten.

## Djupt dyk

Att skriva ut debug-utdata har funnits i programmeringsspråk sedan deras begynnelse och PHP är inget undantag. 

Alternativ till att använda `var_dump()` innefattar `print_r()` som bara presenterar information i en human-readable format, och `debug_zval_dump()`, som också visar interna zend värden.

Vid implementering bör du vara medveten om att debug-utdata kan innehålla känslig information. Därför bör det aldrig användas i produktionsmiljöer. Använd istället debuggverktyg som Xdebug, som ger omfattande debuggningsfunktioner utan några säkerhetsrisker.

## Se också:
1. [PHP: Debugging i PHP - Manual](https://www.php.net/manual/en/debugger.php)
2. [PHP: var_dump - Manual](https://www.php.net/manual/en/function.var-dump.php)
3. [PHP: print_r - Manual](https://www.php.net/manual/en/function.print-r.php)
4. [PHP: debug_zval_dump - Manual](https://www.php.net/manual/en/function.debug-zval-dump.php)
5. [Xdebug - Debugger and Profiler Tool for PHP](https://xdebug.org/)