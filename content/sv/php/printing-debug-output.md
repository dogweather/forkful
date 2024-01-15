---
title:                "Felsökningsutskrift"
html_title:           "PHP: Felsökningsutskrift"
simple_title:         "Felsökningsutskrift"
programming_language: "PHP"
category:             "PHP"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/printing-debug-output.md"
---

{{< edit_this_page >}}

## Varför

Att skriva ut felsökningsinformation i sin kod är ett vanligt förekommande tillvägagångssätt för att hitta och lösa problem i ett PHP-program. Det är ett effektivt verktyg för att identifiera fel och förstå hur ens kod fungerar.

## Hur man gör

För att skriva ut felsökningsinformation i PHP, kan man använda sig av funktionen `var_dump()` eller `print_r()`. Dessa funktioner tar ett argument och skriver sedan ut en detaljerad beskrivning av det värdet. Här är ett exempel:

```
<?php
$namn = "Anna";
var_dump($namn);
?>
```

Resultatet blir:

```
string(4) "Anna"
```

Man kan också använda `echo` för att skriva ut enklare felsökningsmeddelanden. Här är ett exempel:

```
<?php
$ålder = 25;
echo "Min ålder är " . $ålder;
?>
```

Resultatet blir:

```
Min ålder är 25
```

## Djupdykning

När man ska skriva ut felsökningsinformation är det viktigt att tänka på säkerheten. Det kan vara farligt att skriva ut känslig information såsom lösenord eller användaruppgifter. Se därför till att endast skriva ut information som du är bekväm med att visa för andra.

En annan viktig punkt är att inte lämna kvar felsökningskod i produktionsmiljön. Det kan leda till säkerhetsrisker och onödig belastning på servern. Se till att ta bort eller kommentera ut felsökningskoden när den inte längre behövs.

## Se även

* [PHP-funktionen `var_dump()`](https://www.php.net/manual/en/function.var-dump.php)
* [PHP-funktionen `print_r()`](https://www.php.net/manual/en/function.print-r.php)
* [PHP-funktionen `echo`](https://www.php.net/manual/en/function.echo.php)