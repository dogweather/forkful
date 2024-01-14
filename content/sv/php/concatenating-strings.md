---
title:    "PHP: Sammanslagning av strängar"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/php/concatenating-strings.md"
---

{{< edit_this_page >}}

## Varför
När du arbetar med PHP-programmering, kommer du ofta att behöva kombinera flera strängar eller textbitar till en enda sträng. Denna process kallas för strängsammanfogning eller concatenation, och är en viktig del av att bygga dynamiska webbapplikationer.

## Hur man gör det
För att sammanfoga strängar i PHP, använder man sig av operatorn `.` (punkt). Det gör att strängarna slås ihop och blir en enda sträng. Låt oss titta på ett enkelt exempel:

```PHP
$name = "Maria";
$greeting = "Hej " . $name . "! Välkommen till vår hemsida.";
echo $greeting;
```

Output:
Hej Maria! Välkommen till vår hemsida.

Som du kan se, så kombinerar vi här tre strängar men användning av punktoperatorn. Man kan också använda sig av variabler i strängsammanfogning, vilket gör det enkelt och effektivt att skapa dynamisk och personlig text.

## Djupdykning
En intressant egenskap med strängsammanfogning i PHP är att man även kan använda sig av andra datatyper, såsom tal eller boolean-värden. I sådana fall konverteras de automatiskt till en sträng och kan sedan läggas till i concatenation-processen. Detta gör det möjligt att skapa komplexa och dynamiska utskrifter med hjälp av enkla kodrader.

```PHP
$age = 25;
$output = "Jag är " . $age . " år gammal.";
echo $output;
```

Output:
Jag är 25 år gammal.

Det är också viktigt att komma ihåg att vid strängsammanfogning, så sammanfogas strängarna i den ordning som de skrivs i koden. Detta kan ha betydelse när man väljer att använda sig av variabler eller annan data som ligger i en annan ordning än det som man vill ha i den slutliga strängen.

## Se även
- PHP - officiell webbplats: https://www.php.net/
- PHP-konkatenering - dokumentation: https://www.php.net/manual/en/language.operators.string.php
- Enkla kodexempel med strängsammanfogning: https://www.w3schools.com/php/php_operators.asp (på engelska)