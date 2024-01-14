---
title:    "Arduino: Sammanslagning av strängar"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Varför

Att kombinera strängar är en vanlig uppgift när man programmerar med Arduino. Det kan användas för att skapa dynamiska meddelanden, skriva ut data från sensorer eller bara för att göra texten på en skärm mer lättläst. 

## Hur man gör

Att lägga ihop eller konkatenera strängar med Arduino är ganska enkelt. Det finns två huvudsakliga metoder för att göra detta, med hjälp av *sprintf()* och *strcat()* funktionerna.

### *sprintf()*

Med hjälp av *sprintf()* funktionen kan du kombinera flera olika typer av variabler och konstanter tillsammans med en sträng. Här är ett exempel som visar hur du kan kombinera en sträng och en variabel som håller ett värde:

```Arduino
String message = "Temperaturen är %d grader celsius";
int temperature = 25;

sprintf(message, message, temperature);

Serial.println(message); // Output: Temperaturen är 25 grader celsius
```

### *strcat()*

En annan metod är att använda *strcat()* funktionen som används för att lägga till en sträng till slutet av en annan sträng. Här är ett exempel där vi lägger till namnet på en stad till slutet av en sträng:

```Arduino
String message = "Välkommen till ";
String city = "Stockholm";

strcat(message, city);

Serial.println(message); // Output: Välkommen till Stockholm
```

## Djupdykning

När det kommer till att konkatenera strängar, är det viktigt att vara medveten om minnet. Om du lägger till strängar i en loop kan det snabbt ta upp mycket minne, vilket kan leda till minnesbrist och fel i programmet. Det är därför det är viktigt att se till att du inte överskrider den tillgängliga mängden minne när du kombinerar strängar.

## Se även

- [Arduino sprintf() referens](https://www.arduino.cc/reference/en/language/variables/data-types/strings/sprintf/)
- [Arduino strcat() referens](https://www.arduino.cc/reference/en/language/functions/strings/stringobject/strcat/) 
- [Tips for Reducing Arduino Sketch Size](https://www.arduino.cc/en/Guide/Troubleshooting#ought)