---
title:                "PHP: Utskrift av felsökningsresultat"
programming_language: "PHP"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/printing-debug-output.md"
---

{{< edit_this_page >}}

## Varför

Att skriva ut debuggmeddelanden är en viktig del av PHP-programmering. Det gör det enklare att spåra fel och hitta problem i koden. Utan dessa utskrifter kan det vara svårt att förstå varför en kod inte fungerar som den ska.

## Så här gör du

Ett enkelt sätt att skriva ut debuggmeddelanden är att använda funktionen `echo`. Här är ett exempel på hur du kan använda det:

```PHP
<?php
$namn = "Lisa";
echo "Hej " . $namn . "!"; // Utskrift: Hej Lisa!
?>
```

Du kan också använda funktionen `print_r` för att skriva ut en array eller objekt. Här är ett exempel:

```PHP
<?php
$frukter = array("äpple", "banan", "apelsin");
print_r($frukter); // Utskrift: Array ( [0] => äpple [1] => banan [2] => apelsin )
?>
```

För att skriva ut mer detaljerade debuggmeddelanden kan du använda funktionen `var_dump`. Här är ett exempel:

```PHP
<?php
$ålder = 25;
var_dump($ålder); // Utskrift: int(25)
?>
```

## Djupdykning

Det finns flera andra sätt att skriva ut debuggmeddelanden i PHP, som att använda felrapporteringsfunktioner eller logga meddelanden till en fil. Det är också viktigt att bara använda debuggutskrifter under utvecklingsfasen och ta bort dem när koden är klar för produktion.

## Se också

Här är några länkar som kan vara användbara för att lära dig mer om att skriva ut debuggmeddelanden i PHP:

- [PHP manualen för echo](https://www.php.net/manual/en/function.echo.php)
- [PHP manualen för print_r](https://www.php.net/manual/en/function.print-r.php)
- [PHP manualen för var_dump](https://www.php.net/manual/en/function.var-dump.php)
- [En artikel om hur man använder felrapporteringsfunktioner i PHP](https://www.tutorialspoint.com/php/php_error_reporting.htm)
- [En guide om att logga meddelanden till en fil i PHP](https://www.php.net/manual/en/function.error-log.php)