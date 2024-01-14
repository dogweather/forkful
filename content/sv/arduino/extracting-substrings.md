---
title:                "Arduino: Extrahera delsträngar"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/extracting-substrings.md"
---

{{< edit_this_page >}}

## Varför
Substrings, eller delsträngar, är en vanlig del av programmering som är användbar för att söka och manipulera text. Genom att extrahera substrings kan du få ut specifika delar av en textsträng och använda dem för olika ändamål, som att filtrera data eller skapa nycklar för databaser.

## Hur man gör
Det finns flera olika sätt att extrahera substrings i Arduino-programmering. Ett sätt är att använda funktionen "substring()", vilket låter dig ange en startposition och en längd för den delsträng du vill extrahera. Här är ett exempel där vi extraherar de tre första tecknen från en textsträng:

```Arduino
String text = "Hej alla!"; // Skapa en textsträng
String substring = text.substring(0,3); // Extrahera de första tre tecknen
Serial.println(substring); // Skriv ut delsträngen (kommer att visa "Hej")
```

Du kan också använda operatorn "[]" för att extrahera en del av en textsträng. Detta gör du genom att ange önskad position eller intervall inom hakparenteserna. Här är ett exempel där vi extraherar de två sista tecknen från en textsträng:

```Arduino
String text = "Hej alla!";
String substring = text[6]; // Extrahera ett enskilt tecken (inkluderar mellanslag)
Serial.println(substring); // Skriv ut delsträngen (kommer att visa "!")
```

Det går också att använda "indexOf()" funktionen för att hitta en specifik del av en textsträng och sedan extrahera den. Här är ett exempel där vi hittar positionen för mellanslaget och sedan extraherar all text efter det:

```Arduino
String text = "Hej alla!";
int position = text.indexOf(" "); // Hitta positionen för mellanslaget
String substring = text.substring(position + 1); // Extrahera text efter mellanslaget
Serial.println(substring); // Skriv ut delsträngen (kommer att visa "alla!")
```

## Deep Dive
För de som vill fördjupa sig ytterligare i ämnet finns det flera andra sätt att extrahera substrings i Arduino. Det finns till exempel en funktion som heter "charAt()", som låter dig hämta ett tecken baserat på dess position i textsträngen.

Det går också att använda en "for"-loop för att iterera genom en textsträng och extrahera en delsträng för varje steg. Detta kan vara användbart för att hantera längre texter eller om du behöver göra flera olika substrings-extraktioner i samma program.

## Se också
- [Officiell Arduino reference för substring](https://www.arduino.cc/en/Reference/StringSubstring)
- [Tutorial om att använda substrings i Arduino](https://www.arduino.cc/en/Tutorial/StringSubstring)
- [Forumtråd med flera exempel på substring-extraktion i Arduino](https://forum.arduino.cc/index.php?topic=670009.0)