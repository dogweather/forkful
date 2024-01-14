---
title:    "Arduino: Sammansättning av strängar"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/concatenating-strings.md"
---

{{< edit_this_page >}}

## Varför

Concatenating (ihopsättning) av strängar är ett viktigt koncept inom programmering som gör det möjligt att skapa dynamiska och anpassade meddelanden eller instruktioner. Genom att lägga ihop flera strängar kan du skapa en ny, längre sträng som innehåller information från flera olika källor. Detta kan vara till stor hjälp när du arbetar med sensorer, LCD-skärmar eller annan hårdvara som kräver specifika meddelanden.

## Hur man gör

För att concatenate strängar i Arduino, behöver du använda den inbyggda funktionen `String()`. Detta gör det möjligt att skapa en ny sträng genom att kombinera flera andra strängar. Låt oss anta att vi vill skapa ett meddelande som visar temperaturen från en sensor. Vi börjar med att definiera en variabel för vår temperatur som en `float`:

```Arduino
float temp = 25.5;
```
Vi kan sedan använda `String()` för att skapa en ny sträng som kombinerar texten "Temperaturen är" med vår variabel `temp`, som visas nedan:

```Arduino
String meddelande = "Temperaturen är " + String(temp);
```
Resultatet blir en ny sträng, `meddelande`, som ser ut så här:

```
Temperaturen är 25.5
```
Notera att vi använde funktionen `String()` för att konvertera vår `float` till en sträng innan vi lade ihop dem.

## Djupdykning

En annan viktig aspekt av ihopläggning av strängar är att det kan vara särskilt användbart när man arbetar med flerspråkiga projekt. Genom att lägga ihop olika strängar baserat på språk kan du enkelt skapa dynamiska meddelanden på olika språk baserat på dina behov.

Det är också viktigt att komma ihåg att när du arbetar med större och mer komplicerade projekt, kan det vara mer effektivt att använda "char arrays" istället för `String`. Detta kan minimera minnesanvändningen och göra din kod mer lättläst och snabbare.

## Se även

- [Official Arduino documentation on String](https://www.arduino.cc/reference/en/language/variables/data-types/string/)
- [A tutorial on concatenating strings in Arduino](https://www.tutorialspoint.com/arduino/arduino_strings.htm)
- [A video tutorial on using `String()`](https://www.youtube.com/watch?v=gympLD6iOXo)