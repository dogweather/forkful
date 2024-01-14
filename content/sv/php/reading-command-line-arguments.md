---
title:                "PHP: Läsning av kommandoradsargument"
simple_title:         "Läsning av kommandoradsargument"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Varför
Att läsa och förstå kommandoradsargument kan vara en viktig del av programmering i PHP. Det gör det möjligt att interagera med användare och ta emot inmatning på ett dynamiskt sätt.

## Hur man gör
För att läsa kommandoradsargument i PHP kan man använda sig av funktionen ```getopt()```. Detta gör det möjligt att definiera vilka argument som ska läsas och på vilket sätt de ska hanteras. Ett exempel på hur man kan använda funktionen i praktiken:

```
<?php

// Definiera vilka argument som ska läsas och deras format
$options = getopt("f:l:");

// Hämta argumenten och spara dem till variabler
$firstName = $options['f'];
$lastName = $options['l'];

// Skriv ut en hälsning baserat på argumenten som har skickats in
echo "Hej " . $firstName . " " . $lastName . "! Välkommen till min blogg!";
```
Om man nu kör detta skript i terminalen med argumenten ```-f Peter -l Parker```, så kommer följande text att skrivas ut i terminalen:
```
Hej Peter Parker! Välkommen till min blogg!
```
Detta är ett enkelt exempel på hur man kan läsa och använda kommandoradsargument i PHP. Man kan också definiera olika flaggor för att hantera olika typer av argument och säkerställa att de uppfyller vissa krav. För mer information om hur man använder ```getopt()```, se PHP:s dokumentation.

## Utforska vidare
Att läsa kommandoradsargument kan vara användbart i många olika situationer, som att skapa en kommandoradsapplikation, hantera olika användaruppgifter eller automatiskt köra scripts med olika argument. För att lära dig mer om hur man kan använda kommandoradsargument i PHP, kolla in några av följande länkar:

- [PHP:getopt](https://www.php.net/manual/en/function.getenv.php)
- [PHP: Command Line Usage](https://www.php.net/manual/en/features.commandline.usage.php)
- [PHP: Command Line Interface](https://www.php.net/manual/en/features.commandline.php)

## Se även
Här är några andra användbara resurser för att utforska mer om PHP-programmering:

- [PHP-dokumentation](https://www.php.net/manual/en/)
- [W3Schools PHP Tutorial](https://www.w3schools.com/php/)
- [PHP Programmering på YouTube](https://www.youtube.com/watch?v=KX5H9FoTkrA)