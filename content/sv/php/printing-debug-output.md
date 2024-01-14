---
title:    "PHP: Utskrift av felsökningsutdata"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/php/printing-debug-output.md"
---

{{< edit_this_page >}}

## Varför

Att skriva ut felsökningsutgångar är en av de mest grundläggande och användbara teknikerna inom PHP-programmering. Det ger dig möjlighet att se vilken kod som körs och vilka värden variablerna har vid en viss punkt i din kod. Detta hjälper dig att identifiera och lösa eventuella problem som kan uppstå.

## Hur man gör

För att skriva ut en felsökningsutgång i PHP, använd bara funktionen `print_r()` eller `var_dump()`. Dessa funktioner visar informationen om en variabel eller ett objekt.

```PHP
<?php
$frukt = array('äpple', 'banan', 'apelsin');
print_r($frukt);
```

**Output:**

```PHP
Array
(
    [0] => äpple
    [1] => banan
    [2] => apelsin
)
```

```PHP
<?php
$person = array('namn' => 'Anna', 'ålder' => 25, 'land' => 'Sverige');
var_dump($person);
```

**Output:**

```PHP
array(3) {
  ["namn"]=>
  string(4) "Anna"
  ["ålder"]=>
  int(25)
  ["land"]=>
  string(7) "Sverige"
}
```

Dessa funktioner är särskilt användbara för att felsöka större och mer komplexa variabler och objekt.

## Djupdyka

Utöver att bara skriva ut värden på variabler, kan du också skriva ut felsökningsutgångar på olika platser i din kod för att följa hur värdena ändras och för att identifiera eventuella problem som uppstår. Du kan också använda `die()` -funktionen för att stoppa exekveringen av koden och skriva ut en felsökningsutgång på ett visst ställe.

Du kan också formatera din felsökningsutgång för att göra den lättare att läsa genom att använda HTML-taggar eller funkcionaliteten `print_r()` och `var_dump()` som tillåter att du skriver ut värden som element i en tabell.

## Se även

- [PHP Debugging with xdebug](https://www.php.net/manual/en/debugger.php)
- [Debugging PHP Code on the Command Line](https://www.sitepoint.com/debugging-php-code-command-line/)
- [PHP Debugging good practices](https://www.zend.com/blog/php-debugging-good-practices)