---
title:                "Skriva ut felsökningsresultat"
html_title:           "Fish Shell: Skriva ut felsökningsresultat"
simple_title:         "Skriva ut felsökningsresultat"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/printing-debug-output.md"
---

{{< edit_this_page >}}

## Vad och Varför?
Utskrift av felsökningsutdata, eller "debugging", handlar om att skriva värden till seriell utdata för att spåra problem. Programmers gör det för att begripa och fixa fel mer effektivt.

## Hur till?
Koden nedan visas hur du kan göra det i Arduino:

```Arduino
void setup() {
  Serial.begin(9600);
}

void loop() {
  Serial.println("Hej världen!");
  delay(2000);
}
```

Efter du har laddat upp denna kod till din Arduino kommer "Hej världen!" att skrivas på seriell monitor varannan sekund.

## Djup Dykning
Har du någonsin undrat varför vi använder 9600 i `Serial.begin(9600)`? Det beror på att 9600 är standard baudhastigheten för seriell kommunikation och har varit så sedan de tidiga dagarna av datorer. 

Alternativ till `Serial.println()` inkluderar `Serial.print()` och `Serial.write()`. `Serial.print()` är bara som `Serial.println()` men utan radbrytning på slutet. `Serial.write()` skickar dock bytes till seriellt port, vilket gör det användbart för att skicka binär data.

När det gäller implementeringsdetaljer, `Serial.begin(9600)` öppnar seriellt port och bestämmer datahastigheten till 9600 bps. `Serial.println("Hej världen!")` skriver sedan tecknen i strängen till porten, följt av en radbrytning och radåtergång.

## Se Även
För vidare läsning, se följande länkar (alla på engelska):
- [Arduino Serial](https://www.arduino.cc/reference/en/language/functions/communication/serial/)
- [Arduino Debugging Techniques](https://learn.sparkfun.com/tutorials/serial-debugging-techniques)
- [Baud Rate](https://learn.sparkfun.com/tutorials/serial-communication#baud-rate)