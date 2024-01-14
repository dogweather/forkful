---
title:    "PHP: Utskrift av felavruggning"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Varför

Felsökning är en viktig del av PHP-programmering eftersom den hjälper utvecklare att hitta och åtgärda eventuella fel i koden. Att skriva ut debuggmeddelanden kan vara ett värdefullt verktyg för att spåra och förstå hur koden fungerar i realtid.

## Så här gör man

För att skriva ut debuggmeddelanden i PHP använder man funktionen `print_r()`. Denna funktion tar in en variabel som argument och skriver ut dess innehåll i ett läsbart format.

Exempel:

```PHP 
$namn = "Johanna";
print_r($namn);
```

Output: `Johanna`

Man kan också använda sig av `var_dump()`-funktionen för att få en mer detaljerad utskrift av variabeln, inklusive dess typ och längd.

Exempel:

```PHP
$nummer = 21;
var_dump($nummer);
```

Output: `int(21)`

Genom att använda dessa funktioner kan man enkelt spåra och identifiera felaktiga variabler eller värden i koden.

## Djupdykning

För mer avancerad felsökning kan man även använda sig av `error_log()`-funktionen, som skriver ut ett meddelande i PHP:s felloggfil. Detta kan vara särskilt användbart när man försöker felsöka koden på en webbserver.

Exempel:

```PHP
error_log("Ett fel har inträffat!");
```

Genom att genomsöka felloggfilen kan man få mer detaljerad information om var och när felet inträffade, vilket kan hjälpa till att snabba på felsökningen.

## Se också

* [PHP Debugging Handbook](https://phpdebugging.com/)
* [Debugging in PHP: A Comprehensive Guide](https://www.codeofaninja.com/2015/04/php-debugging-guide-steps.html)
* [PHP: error_log - Manual](https://www.php.net/manual/en/function.error-log.php)