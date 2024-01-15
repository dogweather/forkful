---
title:                "Att starta ett nytt projekt"
html_title:           "Arduino: Att starta ett nytt projekt"
simple_title:         "Att starta ett nytt projekt"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Varför
Har du någonsin velat skapa dina egna elektroniska apparater och projekt? Med Arduino kan du göra precis det! Om du är nybörjare är det enkelt att komma igång och om du är en erfaren programmerare finns det utmaningar för att utveckla dina färdigheter.

## Hur du kommer igång
Det första du behöver göra är att skaffa en Arduino-bräda och en dator med Arduino-programvaran installerad. Anslut sedan brädan till datorn och öppna programvaran. För att testa att allt fungerar som det ska kan du köra ett enkelt blinkande LED-program. Kopiera koden nedan och klistra in den i Arduino-programmet.

```arduino
void setup(){
  pinMode(LED_BUILTIN, OUTPUT);  
}

void loop(){
  digitalWrite(LED_BUILTIN, HIGH);  
  delay(1000);                      
  digitalWrite(LED_BUILTIN, LOW);    
  delay(1000);                      
}
```

När du har kopierat in koden kan du trycka på knappen "Verify" för att kontrollera om det är några fel i koden. Om allt är som det ska kan du sedan ladda upp programmet genom att klicka på knappen "Upload". Nu borde LED-lampan på brädan börja blinka med en sekunds mellanrum.

## Djupdykning
För att starta ett nytt projekt med Arduino behöver du först ha en idé på vad du vill skapa. Det finns massor av projektidéer online som du kan inspireras av. Efter att du har en idé är det dags att bestämma vilka komponenter och sensorer du behöver. Du kan enkelt köpa dessa online eller i en elektronikbutik.

När du har alla dina komponenter är det dags att sätta igång med att skapa koden. Det finns massor av resurser online för att hjälpa dig att lära dig kodning på Arduino, inklusive officiella dokumentationen och olika forum där du kan ställa frågor och få hjälp. Var inte rädd för att testa och experimentera för att lära dig mer och utveckla dina färdigheter.

## Se även
- [Officiell Arduino-hemsida](https://www.arduino.cc/)
- [Arduino-projekt på Instructables](https://www.instructables.com/arduino/projects/)
- [Arduino-forum på Reddit](https://www.reddit.com/r/arduino/)