---
title:                "Att använda en debugger"
aliases:
- /sv/php/using-a-debugger.md
date:                  2024-01-26T03:50:37.644178-07:00
model:                 gpt-4-0125-preview
simple_title:         "Att använda en debugger"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/using-a-debugger.md"
---

{{< edit_this_page >}}

## Vad & Varför?
En debugger är ett verktyg som hjälper programmerare att förstå vad deras kod faktiskt gör när den körs. Det är förstoringsglaset som låter oss zooma in på buggar—de där irriterande problemen som får våra program att krascha eller spotta ut felaktiga svar—och krossa dem. Vi använder debuggers för att de sparar oss timmar av utskriftsuttalanden och gissningslekar.

## Hur man gör:
PHP levereras med en interaktiv debugger som kallas Xdebug. Så här använder du den.

Först, se till att du har Xdebug installerat och konfigurerat i din `php.ini`-fil:

```
zend_extension=/usr/local/lib/php/extensions/no-debug-non-zts-xxxxxxxx/xdebug.so
xdebug.mode=debug
xdebug.start_with_request=yes
```

Nästa, skriv ett enkelt PHP-skript med en bugg:

```PHP
<?php
function add($a, $b) {
    return $a - $b; // Hoppsan! Detta borde vara ett plus, inte ett minus
}

$resultat = add(1, 2);
echo "Resultatet är: $resultat"; // Utmatningen borde vara 3, inte -1
```

När du använder en IDE som PhpStorm, sätt en brytpunkt genom att klicka bredvid radnumret. Kör debuggern och se hur variablerna ändras när du stegar igenom exekveringen. När du stegar över `add`-funktionen kommer du att märka att `$resultat` blir -1, vilket är oväntat.

## Djupdykning:
Historiskt sett användes PHP främst för små skript, och felsökning var en fråga om att lägga till `var_dump()` och `print_r()`-uttalanden genom koden. Över tid, med PHP som blir en nyckelspelare i webbutveckling, började man använda mer sofistikerade verktyg som Xdebug och Zend Debugger.

Alternativ till Xdebug inkluderar pcov och phpdbg. Dessa erbjuder olika funktioner men kanske inte är lika fullständigt utrustade som Xdebug. phpdbg är en lättviktig, PHP-specifik debugger som distribueras med PHP sedan version 5.6, och pcov är en kodtäckningsdrivrutin.

När du implementerar en debugger, kom ihåg att du aldrig ska lämna debuggern påslagen på din produktionsserver, eftersom det kan exponera säkerhetsbrister och sänka prestandan.

## Se också:
- [Xdebug-dokumentation](https://xdebug.org/docs/)
- [PhpStorms felsökningsguide](https://www.jetbrains.com/help/phpstorm/debugging.html)
- [PHP.net om phpdbg](https://www.php.net/manual/en/book.phpdbg.php)
- [pcov på GitHub](https://github.com/krakjoe/pcov)
