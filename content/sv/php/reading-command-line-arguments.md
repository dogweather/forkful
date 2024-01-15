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

## Varför
I detta artikel kommer du att lära dig om hur du läser kommandoradsargument i PHP och varför detta är ett användbart verktyg för att hantera användarinput. Genom att läsa kommandoradsargument kan du enkelt ta emot och bearbeta data från användaren.

## Hur man gör det
Först och främst behöver du känna till kommandot "argv" i PHP, som är en array som innehåller alla kommandoradsargument som skickas med när man kör ett PHP-skript. Här är ett exempel på hur du kan använda detta i ditt kod:

```PHP
<?php
    // Skriv ut alla kommandoradsargument
    var_dump($argv);
    
    // Skriv ut det första argumentet
    echo $argv[0];
?>
```

Om du till exempel kör ovanstående kod med kommandot "php script.php arg1 arg2" kommer den första raden att skriva ut en array med alla argumenten och den andra raden kommer att skriva ut "arg1". Detta ger dig tillgång till all input som användaren skickar med när de kör ditt skript.

## Deep Dive
För att få en bättre förståelse för hur du kan använda kommandoradsargument i dina projekt, så kan du också utforska hur du kan använda olika PHP-funktioner som "getopt()" och "filter_var()". Dessa funktioner ger dig möjligheten att hantera och filtrera kommandoradsargument på ett mer avancerat sätt. Genom att känna till dessa funktioner kan du skapa mer robusta och säkrare skript.

## Se även
För mer information om kommandoradsargument i PHP, se följande länkar:
- [PHP manual om kommandoradsargument](https://www.php.net/manual/en/reserved.variables.argv.php)
- [getopt() funktionen](https://www.php.net/manual/en/function.getopt.php)
- [filter_var() funktionen](https://www.php.net/manual/en/function.filter-var.php)