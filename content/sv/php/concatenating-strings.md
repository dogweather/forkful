---
title:                "Slå ihop strängar"
html_title:           "PHP: Slå ihop strängar"
simple_title:         "Slå ihop strängar"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/concatenating-strings.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att sammanfoga eller "concatenate" strängar är en vanlig uppgift för programmerare, där man lägger ihop flera textsträngar till en enda. Detta kan vara användbart när man vill skapa dynamiska textutskrifter eller strängar för datahantering.

## Så gör du:
För att sammanfoga strängar i PHP använder man sig av operatorn ".", vilket står för "concatenation". Detta tillåter oss att kombinera flera strängar och skapa en ny. Till exempel:

```PHP
$first_name = "Anna";
$last_name = "Svensson";
$full_name = $first_name . " " . $last_name;

echo $full_name; // kommer att skriva ut "Anna Svensson"
```

Här använder vi operatorn "." för att slå ihop strängarna från variablerna $first_name och $last_name, och lägger även till ett mellanslag mellan dem.

## Djupdykning:
Sammanfogande av strängar är en viktig del av nästan alla programmeringspråk, inte bara i PHP. Detta koncept kommer från matematiken, där "+" representerar addition och "." representerar sammanslagning. I tidigare versioner av PHP användes även funktionen "strcat()" för att sammanfoga strängar, men detta har ersatts av operatorn "." sedan version 4.0.0.

Det finns också alternativ till att använda operatorn ".", såsom funktionen "sprintf()", som är speciellt användbar när man behöver formatera text dynamiskt eller skapa komplexa strängar.

När man sammanfogar strängar i PHP, så behandlas alla värden som strängar, även om de ursprungligen var nummer. Detta kan ibland leda till oväntade resultat, så det är viktig att vara medveten om detta när man använder operatorn ".".

## Se även:
Läs mer om sammanfogande av strängar och andra string operationer i PHP på [PHP:s officiella dokumentation](https://www.php.net/manual/en/language.operators.string.php).