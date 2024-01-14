---
title:                "Arduino: Utskrift av felsökningsutdata"
simple_title:         "Utskrift av felsökningsutdata"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/printing-debug-output.md"
---

{{< edit_this_page >}}

## Varför

Att skriva debug-utdata är en viktig del av programmering med Arduino. Det kan hjälpa dig att förstå hur din kod fungerar och hitta eventuella problem som kan dyka upp.

## Såhär gör du

För att skriva debug-utdata så behöver du använda funktionen `Serial.print ()` i dina Arduino-program. Den här funktionen skriver ut text eller numeriska värden till serieporten på din Arduino. Här är ett enkelt exempel på hur du kan skriva ut text och ett numeriskt värde:

```
Arduino
void setup() {
  Serial.begin(9600);
}

void loop() {
  Serial.print("Hello world!");
  int value = 42;
  Serial.print("The answer is: ");
  Serial.print(value);
  delay(1000); //vänta en sekund innan nästa utskrift
}
```

I det här exemplet kan du se att vi först behöver initiera serieporten med funktionen `Serial.begin ()` i `setup ()`-delen av koden. Sedan använder vi `Serial.print ()` för att skriva ut text och numeriska värden. Det är viktigt att inkludera en liten fördröjning (eng. delay) mellan utskrifterna så att seriell kommunikation inte överbelastas.

## Djupdykning

När du skriver debug-utdata kan du använda dig av flera olika funktioner än bara `Serial.print ()`. Här är några andra användbara funktioner att känna till:

- `Serial.println ()` - samma som `Serial.print ()`, men lägger till en ny rad efter varje utskrift. Perfekt för att göra det lättare att läsa debug-utdata.
- `Serial.write ()` - skickar en byte istället för en sträng. Användbar för att skicka binära data.
- `Serial.available ()` - kollar hur många tecken som finns tillgängliga att läsa från serieporten. Användbar för att undvika att läsa tom data.
- `Serial.read ()` - läser ett tecken från serieporten och returnerar det som en byte. Användbar för att läsa indata från andra enheter.

Det finns också flera inställningar du kan ändra för serieporten, som baud rate och data bits. Detta kan vara användbart om du har problem med att data inte skrivs korrekt till serieporten.

## Se även

Här är några andra resurser som kan vara användbara när du skriver debug-utdata:

- [Arduino Serial Communication](https://www.arduino.cc/reference/en/language/functions/communication/serial/) - officiell dokumentation om hur man använder serieporten i Arduino.
- [Serial.print () vs Serial.println () in Arduino](https://www.teachmemicro.com/arduino-serialprint-vs-serialprintln/) - en förklaring av skillnaden mellan `Serial.print ()` och `Serial.println ()`.
- [Arduino Serial Monitor](https://www.arduino.cc/en/Tutorial/SerialMonitor) - en guide om hur man använder den inbyggda seriemonitorn i Arduino IDE för att visa och skicka seriell data mellan din dator och Arduino.

Förhoppningsvis har du nu en bättre förståelse för varför och hur man skriver debug-utdata i Arduino. Lycka till med dina projekt!