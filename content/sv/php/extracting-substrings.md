---
title:                "PHP: Extrahera substrängar"
programming_language: "PHP"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/extracting-substrings.md"
---

{{< edit_this_page >}}

## Varför

Att extrahera substrängar är en vanlig utmaning i PHP-programmering. Det kan vara användbart för att manipulera textdata eller hantera dynamisk input från användare. Genom att lära sig hur man extraherar substrängar kan du utöka dina PHP-färdigheter och skapa mer effektiv kod.

## Så här gör du

För att extrahera en substräng från en befintlig sträng i PHP, använder du funktionen `substr()`. Syntaxen för denna funktion är:

```PHP
$nyStrang = substr($befintligStrang, startposition, längd);
```

Det första argumentet är den befintliga strängen som du vill extrahera en del av. Det andra argumentet är startpositionen för substrängen, och det sista argumentet är längden på substrängen. Om du inte anger ett längdargument kommer substrängen att extraheras från startpositionen till slutet av den befintliga strängen.

Här är ett exempel på hur du kan använda `substr()` för att extrahera en del av en sträng:

```PHP
$strang = "Hej, välkommen till min blogg!";
$delAvStrang = substr($strang, 5, 8);

echo $delAvStrang; // kommer att skriva ut "välkommen"
```

Du kan också använda `substr()` för att extrahera från slutet av en sträng genom att använda negativa startpositioner. Här är ett exempel:

```PHP
$strang = "Hej, välkommen till min blogg!";
$delAvStrang = substr($strang, -6, 6);

echo $delAvStrang; // kommer att skriva ut "blogg!"
```

## Djupdykning

När du extraherar substrängar finns det några saker att tänka på. Först och främst är det viktigt att förstå att indexeringen av en sträng i PHP börjar på 0. Det innebär att den första bokstaven i en sträng har index 0, den andra bokstaven har index 1 och så vidare.

Dessutom kommer `substr()` inte att generera ett fel om den angivna startpositionen är utanför gränserna för den befintliga strängen. Istället kommer den att returnera en tom sträng. Du bör alltid kontrollera om längden på den befintliga strängen är längre än din startposition för att undvika oväntade resultat.

## Se även

- PHP:dokumentation om substrängar (https://www.php.net/manual/en/function.substr.php)
- En guide till strängfunktioner i PHP (https://www.w3schools.com/php/php_string_functions.asp)
- En videohandledning om att extrahera substrängar i PHP (https://www.youtube.com/watch?v=dSKJvjefE3k)