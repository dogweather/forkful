---
title:    "Arduino: Extrahera delsträngar"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/extracting-substrings.md"
---

{{< edit_this_page >}}

## Varför

Har du någonsin behövt få ut en del av en textsträng och undrat hur du kan göra det med Arduino-programmering? Att extrahera substrängar kan vara användbart för att bearbeta och manipulera data, till exempel från sensorer eller användarinmatning. Det kan också öppna upp för möjligheter att skapa mer dynamiska användargränssnitt. Läs vidare för att lära dig hur du kan få ut substrängar med hjälp av Arduino.

## Hur man gör det

Att extrahera substrängar med Arduino är faktiskt ganska enkelt. Först behöver du en textsträng som du vill extrahera en del av, och sedan behöver du veta vilken del du vill få ut. Det kan till exempel vara ett visst antal tecken, eller ett visst område av texten. Här är en kodexempel på hur du kan extrahera en del av en textsträng:

```Arduino
String text = "Hej alla! Välkommen till min blogg.";
String del = text.substring(11, 18);
Serial.println(del);
```

I detta exempel extraheras texten "Välkommen" från den ursprungliga textsträngen och skickas sedan till en serieport för att kunna visas i en mottagare, till exempel en datorskärm. Om du kör denna kod kommer du att få ut "Välkommen" i serieporten och se det på skärmen.

## Djupdykning

För att förstå mer om hur substrängsextraktion fungerar i Arduino, är det viktigt att förstå parametrarna som används i `.substring()`-funktionen. Den första parametern är startindexet, det vill säga var substrängen ska börja. I vårt exempel ovan är startindex 11, vilket motsvarar bokstaven "V" i ordet "Välkommen". Den andra parametern är slutindexet, där substrängen ska sluta. I vårt exempel är slutindexet 18, vilket motsvarar bokstaven "n" i ordet "Välkommen". Notera att slutindexet inte är en del av den extraherade substrängen, så du måste lägga till ett till slutindex för att få med det sista tecknet.

En annan användbar funktion för substrängar är `.startsWith()`, som returnerar en boolsk (sann/falsk) värde beroende på om en textsträng börjar med en viss del av en text. Låt oss se ett exempel:

```Arduino 
String text = "Hej alla! Välkommen till min blogg.";
if(text.startsWith("Hej")) {
  //kör kod om texten börjar med "Hej"
}
```

Denna kod kommer att kolla om texten börjar med "Hej" och om så är fallet, så kommer koden inuti if-satsen att köras. Detta kan vara användbart för att filtrera och bearbeta inkommande data från till exempel en sensor.

## Se också

Här är några andra användbara resurser för att lära dig mer om substrängsextraktion med Arduino:

- [Arduino-officiell dokumentation om `.substring()`](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/substring/)
- [Tutorial: Extrahera substrängar med Arduino](https://www.teachmemicro.com/substring-extraction-arduino/)
- [Video: Strängar och substrängar i Arduino](https://www.youtube.com/watch?v=6Llt1WNELIA)

Lycka till med att extrahera substrängar i dina Arduino-projekt! Glöm inte att experimentera och utforska för att lära dig ännu mer.