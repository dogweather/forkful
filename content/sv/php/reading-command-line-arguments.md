---
title:                "Läsning av kommandoradsargument"
html_title:           "PHP: Läsning av kommandoradsargument"
simple_title:         "Läsning av kommandoradsargument"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Vad & Varför? 
Att läsa kommandoradsargument är en vanlig uppgift som programmerare utför för att interagera med sina program på ett mer dynamiskt sätt. Genom att läsa in argument som användaren skriver in i terminalen kan en programmerare skapa anpassade och mer interaktiva program som kan hantera olika användarfall.

## Så här gör du:
För att läsa in kommandoradsargument i PHP använder vi en inbyggd funktion vid namn `$_SERVER['argv']`. Denna funktion returnerar en array som innehåller alla argument som lästs in. Nedan följer ett exempel på hur detta kan implementeras:

```PHP
// Filnamn: test.php
$args = $_SERVER['argv']; // Läser in användarens argument som en array
echo "Dina argument är: ";
print_r($args); // Skriver ut argumenten i terminalen
```
Om du nu kör detta program från kommandoraden och lägger till några argument, kommer du att få utskriften av dessa argument:

```
php test.php argument1 argument2
Dina argument är: 
Array
(
    [0] => test.php
    [1] => argument1
    [2] => argument2
)
```

## Djupdykning:
Att läsa in kommandoradsargument är en vanlig uppgift inom programmering, och detta har funnits som en funktion i PHP sedan dess första version. Det finns olika sätt att implementera detta på, men vanligtvis är det `$_SERVER['argv']` som används. Alternativ till detta är att använda andra funktioner som `getopt()` eller `argv_parse()` för att läsa in argumenten i en mer strukturerad form.

## Se även:
- Mer information om `$_SERVER['argv']`: https://www.php.net/manual/en/reserved.variables.argv.php 
- Alternativa funktioner för att läsa kommandoradsargument: https://stackoverflow.com/questions/2186391/how-can-i-read-command-line-arguments-in-a-php-script