---
title:                "Arduino: Att påbörja ett nytt projekt"
programming_language: "Arduino"
category:             "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Varför

Om du älskar elektronik eller programmering, är Arduino ett utmärkt verktyg för att kombinera båda dina intressen. Med en Arduino-mikrokontroller kan du skapa otroliga elektroniska projekt, från enklare LED-lampor till mer avancerade robotar.

## Hur man gör

För att komma igång behöver du en Arduino-mikrokontroller, ett USB-kabel och en dator. Du kan köpa en grundläggande Arduino starter kit som innehåller allt du behöver för dina första projekt.

### Blinka en LED-lampa

Första steget i att lära sig Arduino-programmering är att blinka en LED-lampa. Här är en enkel kod för att göra det:

```Arduino
// Tilldela LED-lampans pin-nummer
int ledPin = 13;

void setup() {
  // Sätt upp pin-numret som en utgång
  // Det betyder att vi ger en signal ut genom den
  pinMode(ledPin, OUTPUT);
}

void loop() {
  // Tänd LED-lampan
  digitalWrite(ledPin, HIGH);
  // Vänta i en sekund
  delay(1000);
  // Släck LED-lampan
  digitalWrite(ledPin, LOW);
  // Vänta i en sekund
  delay(1000);
}
```

Om du laddar upp koden på din Arduino, borde du se LED-lampan blinka med en sekunds mellanrum.

### Läs analoga värden från en potentiometer

En annan användbar funktion i Arduino är att kunna läsa analoga värden från sensorer som potentiometrar. Här är ett exempel på hur du kan göra det:

```Arduino
// Tilldela potentiometerns pin-nummer
int potPin = A0;

void setup() {
  // Sätt upp serieläsning för att kunna läsa av värden från potentiometern
  Serial.begin(9600);
}

void loop() {
  // Läs av värdet från potentiometern
  int potValue = analogRead(potPin);
  // Skriv ut värdet i seriell monitor
  Serial.println(potValue);
}
```

Om du öppnar seriell monitor i Arduino IDE, borde du se analoga värden från potentiometern visas när du justerar den.

## Utforska mer

Det finns många resurser tillgängliga för att hjälpa dig lära dig mer om Arduino-programmering och elektronik i allmänhet. Du kan hitta tutorial-videor på YouTube, forum där du kan ställa frågor och få hjälp och många böcker på biblioteket eller på nätet.

I början kan det vara överväldigande att starta ett nytt projekt, så en bra idé är att börja med enkla kretsar och sedan gradvis utöka dina kunskaper och projekt. Det finns också massor av färdiga projekt som du kan hitta online med detaljerade instruktioner för hur du kan bygga dem.

## Se även

Här är några användbara länkar för att komma igång med Arduino:

- Officiell Arduino hemsida (https://www.arduino.cc/)
- Arduino IDE (https://www.arduino.cc/en/Main/Software)
- Arduino forum (https://forum.arduino.cc/)
- Arduino tutorial på YouTube (https://www.youtube.com/watch?v=pLcn5vCbZBM)