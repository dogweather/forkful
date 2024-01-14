---
title:                "PHP: Utskrift av felsökningsresultat"
simple_title:         "Utskrift av felsökningsresultat"
programming_language: "PHP"
category:             "PHP"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/printing-debug-output.md"
---

{{< edit_this_page >}}

## Varför

Att skriva kod kan vara en komplex process och ibland kan det vara svårt att felsöka och hitta fel i koden. En av de mest användbara sätten att felsöka din PHP-kod är genom att skriva ut felsökningsmeddelanden. Detta ger dig en djupare insikt i vad som händer i din kod och kan hjälpa dig att hitta och lösa problem snabbare.

## Så här gör du

För att skriva ut debug-utdata i din PHP-kod, kan du använda funktionen ```print_r ()``` eller ```var_dump ()```. Dessa funktioner visar en detaljerad utskrift av variabler eller objekt som du väljer att felsöka.

Här är ett enkelt exempel på hur du kan använda ```print_r ()``` för att felsöka en array i din kod:

```
<?php
$array = ['Apple', 'Orange', 'Banana'];
print_r($array);
?>
```

Detta kommer att producera följande utskrift:

```
Array
(
    [0] => Apple
    [1] => Orange
    [2] => Banana
)
```

Som du kan se ger detta en detaljerad utskrift av alla element i arrayen, vilket gör det lättare att hitta eventuella fel eller problem.

Å andra sidan kan ```var_dump ()``` ge dig ännu mer information om variabler eller objekt, inklusive datatyper, storlek och värden. Så här kan du använda ```var_dump ()``` för att felsöka en variabel:

```
<?php
$name = 'Emma';
var_dump($name);
?>
```

Denna kod kommer att producera följande utskrift:

```
string(4) "Emma"
```

Som du kan se visar ```var_dump ()``` datatypen och längden på strängen, vilket kan vara mycket användbart vid felsökning av mer komplexa variabler.

## Djupdykning

En annan metod för att skriva debug-utdata är att använda ```error_log ()```-funktionen. Istället för att skriva ut direkt på skärmen, kan du skicka felsökningsmeddelanden till en loggfilsfil. Detta är särskilt användbart om du vill felsöka en webbapplikation som kör på en server utan direkt åtkomst till en konsol eller terminal.

Till exempel kan du använda ```error_log ()``` för att logga ett meddelande när en viss kodruta körs:

```
<?php
error_log("Kodruta körs!");
?>
```

Meddelandet sparas sedan i en loggfil som du kan granska senare för att hitta eventuella fel eller problem.

## Se även

- [PHP - Debuga din kod](https://www.php.net/manual/en/debugger.php)
- [PHP - Felrapportering](https://www.php.net/manual/en/errorfunc.configuration.php)
- [PHP - Felloggning](https://www.php.net/manual/en/function.error-log.php)