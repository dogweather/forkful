---
title:    "Arduino: Att påbörja ett nytt projekt"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Varför

Funderar du på att starta ett nytt projekt med Arduino men vet inte var du ska börja? Fortsätt läsa! I den här bloggposten kommer jag att dela med mig av några tips och tricks för att komma igång med Arduino programmering.

## How To (Så här)

Att programmera med Arduino är enkelt och roligt. Här är ett exempel på hur man kan blinka en LED-lampa med hjälp av en Arduino Uno:

```
Arduino uno;           // Skapar en variabel för att använda Arduino Uno
pinMode(13, OUTPUT);   // Sätter pin 13 som en utgång
while(true){           // En loop som körs hela tiden
  digitalWrite(13, HIGH);  // Sätter pin 13 till "HIGH" (tänd)
  delay(1000);              // Väntar en sekund
  digitalWrite(13, LOW);   // Sätter pin 13 till "LOW" (släckt)
  delay(1000);              // Väntar en sekund igen
}
```

I detta exempel, skapas först en variabel för att representera Arduino Uno. Sedan sätts pin 13 som en utgång och en while-loop skapas som kommer att köra oändligt. Inuti loopen, ändras pin 13 mellan "HIGH" och "LOW" för att skapa en blinkande effekt med en paus på en sekund mellan varje tillstånd.

## Deep Dive (Utforska Djupet)

För att komma igång med ett nytt Arduino projekt, är det viktigt att förstå de grundläggande koncepten för programmering. Arduino språket är baserat på C/C++, så det är en bra idé att bekanta sig med dessa språk innan du börjar.

För att lära dig mer om Arduino programmering, kan du använda Arduinos officiella hemsida som har massor av handledningar, exempel och dokumentation. Du kan också gå med i Arduino forumet där du kan ställa frågor och få hjälp från andra användare.

## Se också (See Also)

- [Arduino officiella hemsida](https://www.arduino.cc/)
- [Arduino förum](https://forum.arduino.cc/)
- [Lär dig C/C++ på Codecademy](https://www.codecademy.com/learn/learn-c-plus-plus)