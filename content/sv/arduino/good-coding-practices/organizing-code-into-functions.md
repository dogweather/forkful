---
date: 2024-01-26 01:08:33.244094-07:00
description: "Hur man g\xF6r: F\xF6rest\xE4ll dig att du vill blinka en LED. Utan\
  \ funktioner \xE4r din `loop` en r\xF6rig r\xF6ra. Med funktioner \xE4r den prydlig.\
  \ S\xE5 h\xE4r g\xF6r du."
lastmod: '2024-03-13T22:44:38.172655-06:00'
model: gpt-4-1106-preview
summary: "F\xF6rest\xE4ll dig att du vill blinka en LED."
title: Att organisera kod i funktioner
weight: 18
---

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
