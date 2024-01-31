---
title:                "Att organisera kod i funktioner"
date:                  2024-01-26T01:08:33.244094-07:00
model:                 gpt-4-1106-preview
simple_title:         "Att organisera kod i funktioner"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att organisera kod i funktioner innebär att dela upp koden i återanvändbara delar, där varje del utför ett specifikt jobb. Programmerare gör detta för att göra koden lättare att läsa, felsöka och återanvända. Det är som att sortera Lego i lådor - det sparar dig från att rota igenom en kaotisk hög varje gång du vill bygga något.

## Hur man gör:
Föreställ dig att du vill blinka en LED. Utan funktioner är din `loop` en rörig röra. Med funktioner är den prydlig. Så här gör du:

```Arduino
const int LED_PIN = 13;

void setup() {
  pinMode(LED_PIN, OUTPUT);
}

void loop() {
  blinkLED(500); // Blinka LED-en var 500:e millisekund
}

// Funktion för att blinka en LED
void blinkLED(int fordröjningsTid) {
  digitalWrite(LED_PIN, HIGH);
  delay(fordröjningsTid);
  digitalWrite(LED_PIN, LOW);
  delay(fordröjningsTid);
}
```

Exempel på resultat: Din LED blinkar glatt, och koden syfte är klart vid en första anblick.

## Fördjupning
Innan funktioner var programmering en linjär bilresa; du såg varje gupp från start till mål. Efter funktioner är det mer som att hoppa på flyg - du hoppar över till de viktiga delarna. Historiskt sett var subrutiner (tidiga funktioner) en revolution inom programmering, vilket lät kodare undvika att upprepa sig själva – det är DRY-principen, Don’t Repeat Yourself (Upprepa inte dig själv). Alternativ till funktioner kan inkludera makron eller användningen av klasser för objektorienterad programmering (OOP). Nitty-gritty? När du definierar en funktion ger du kompilatorn en blåkopia för att utföra en uppgift. Med Arduino definierar du ofta void-funktioner som agerar som enkla kommandon för en mikrokontroller, men funktioner kan också returnera värden, vilket gör dem mer mångsidiga.

## Se också
För mer om funktioner, bläddra igenom dessa:

- Arduinos officiella funktionsreferens: https://www.arduino.cc/reference/en/language/functions/
- Lär dig mer om DRY-principen: https://en.wikipedia.org/wiki/Don%27t_repeat_yourself
- En repetition om historien om subrutiner: https://en.wikipedia.org/wiki/Subroutine
