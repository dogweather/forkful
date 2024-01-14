---
title:                "PHP: Radera tecken som matchar ett mönster"
simple_title:         "Radera tecken som matchar ett mönster"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Varför
Ibland behöver man kanske rensa bort vissa tecken som matchar ett visst mönster i en sträng, till exempel när man vill få bort all formatering från en text för att göra den mer läsbar. Detta kan också vara användbart när man arbetar med databaser och behöver ta bort vissa tecken från strängar eller kolumner.

## Hur man gör det
För att kunna ta bort tecken som matchar ett visst mönster i en sträng behöver man använda sig av PHP:s inbyggda funktion "preg_replace()". Denna funktion tar tre parametrar: mönstret som ska matchas, vad det ska ersättas med och den sträng som ska bearbetas.

Här är ett exempel på hur du kan använda "preg_replace()" för att ta bort alla siffror från en text:

```PHP
$text = "Jag är 27 år gammal.";
$nyText = preg_replace("/[0-9]/", "", $text);
echo $nyText; // skriver ut "Jag är år gammal."
```

Vi använde här ett reguljärt uttryck (regEx) som definierar det mönster som vi vill matcha och ta bort, i detta fall alla siffror från 0 till 9. Den andra parametern i "preg_replace()" är vad vi vill byta ut de matchande tecknen med, i detta fall ingenting. Och den tredje parametern är den sträng som ska bearbetas, i detta fall "Jag är 27 år gammal.".

Man kan också använda "preg_replace()" för att ta bort flera mönster från en sträng. I exemplet nedan tar vi bort alla siffror och specialtecken från en sträng:

```PHP
$text = "1234 Hjälm&hita, 2215#ström";
$nyText = preg_replace("/[0-9\W]/", "", $text);
echo $nyText; // skriver ut "Hjälmita ström"
```

Som ni ser har vi i vårt regEx lagt till "\W" efter sifferranget för att även matcha alla specialtecken.

## Djupdykning
"preg_replace()" har många olika funktioner och möjligheter. Man kan till exempel lägga till flaggor som gör att matchningen blir mer flexibel och inte bara tar bort exakta matchningar. Man kan också använda paranteser i mönstret för att ta ut specifika delar av den matchande strängen och till exempel använda dem för att skapa nya strängar.

Man kan också använda "preg_replace()" för att byta ut tecken eller mönster med andra tecken eller mönster, istället för att bara ta bort dem. Detta kan vara användbart när man vill formatera en text på ett specifikt sätt, som att separera siffror med en bindestreck eller ändra stora bokstäver till små.

## Se även
- PHP:s officiella dokumentation för "preg_replace()": https://www.php.net/manual/en/function.preg-replace.php
- En tutorial om reguljära uttryck och hur man använder dem i PHP: https://www.tutorialrepublic.com/php-tutorial/php-regular-expressions.php
- En annan bloggpost om strängmanipulering i PHP: https://medium.com/@kodius/str%C3%A4ngmanipulering-i-php-408c82de7a8a