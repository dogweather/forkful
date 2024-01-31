---
title:                "Skriva ut felsökningsdata"
date:                  2024-01-20T17:53:05.198950-07:00
model:                 gpt-4-1106-preview
simple_title:         "Skriva ut felsökningsdata"

category:             "PHP"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/printing-debug-output.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Utskrift av felsökningsmeddelanden är en teknik för att visa variabler, stackspårningar och annan data under körning. Programmerare använder detta förfarande för att snabbt lokalisera och åtgärda buggar.

## Hur man gör:
```php
<?php
// Enkel utskrift för att visa värdet på en variabel
$myVariable = "Hej värld!";
echo $myVariable;

// Mer komplex utskrift med var_dump för att visa typer och värden
$debugData = ['a' => 1, 'b' => 2];
var_dump($debugData);

// Snygga till var_dump med <pre>-taggen i HTML
echo '<pre>' . var_export($debugData, true) . '</pre>';
```
Exempel på utmatning:
```
Hej värld!
array(2) {
  ["a"]=>
  int(1)
  ["b"]=>
  int(2)
}
```

## Fördjupning:
Utskrift för felsökning har varit ett verktyg sedan programmeringens barndom, nödvändigt för att förstå vad som händer bakom kodens kulisser. Alternativ till utskriftsdebugging inkluderar att använda programmeringsmiljöer med inbyggda debuggers, loggfiler eller till och med moderna verktyg som Xdebug för PHP. Utskriftsdebugging är ofta enklare och snabba att använda, men kan bli rörigt och oöverskådligt i större projekt.

## Se även:
- PHP:s officiella dokumentation för `var_dump`: https://www.php.net/manual/en/function.var-dump.php
- PHP:s officiella dokumentation för `echo`: https://www.php.net/manual/en/function.echo.php
- Xdebug, en debugger och profiler för PHP: https://xdebug.org/docs
