---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:02:46.872129-07:00
description: 'Hoe te: Stel je voor dat je een LED wilt laten knipperen. Zonder functies,
  is je `loop` een rommelige warboel. Met functies, is het netjes. Hier is hoe.'
lastmod: '2024-03-13T22:44:51.077395-06:00'
model: gpt-4-0125-preview
summary: Stel je voor dat je een LED wilt laten knipperen.
title: Code organiseren in functies
weight: 18
---

## Hoe te:
Stel je voor dat je een LED wilt laten knipperen. Zonder functies, is je `loop` een rommelige warboel. Met functies, is het netjes. Hier is hoe:

```Arduino
const int LED_PIN = 13;

void setup() {
  pinMode(LED_PIN, OUTPUT);
}

void loop() {
  blinkLED(500); // Laat de LED elke 500ms knipperen
}

// Functie om een LED te laten knipperen
void blinkLED(int delayTime) {
  digitalWrite(LED_PIN, HIGH);
  delay(delayTime);
  digitalWrite(LED_PIN, LOW);
  delay(delayTime);
}
```

Voorbeelduitvoer: Je LED knippert vrolijk, en het doel van de code is in één oogopslag duidelijk.

## Diepere Duik
Voor functies was programmeren een lineaire roadtrip; je zag elk gat in de weg van begin tot eind. Na functies is het meer zoals vluchten hoppen - je springt naar de belangrijke delen. Historisch gezien waren subroutines (vroege functies) een revolutie in programmeren, waardoor coders zichzelf niet hoefden te herhalen – dat is het DRY-principe, Don’t Repeat Yourself (Herhaal Jezelf Niet). Alternatieven voor functies kunnen macros bevatten of het gebruik van klassen voor objectgeoriënteerd programmeren (OOP). De kern van de zaak? Wanneer je een functie definieert, geef je de compiler een blauwdruk voor het uitvoeren van een taak. Met Arduino definieer je vaak void functies die dienen als simpele commando's voor een microcontroller, maar functies kunnen ook waardes teruggeven, wat ze veelzijdiger maakt.

## Zie Ook
Voor meer over functies, doorzoek deze:

- Officiële functiereferentie van Arduino: https://www.arduino.cc/reference/en/language/functions/
- Leer meer over het DRY-principe: https://nl.wikipedia.org/wiki/Don%27t_repeat_yourself
- Een opfrissing over de geschiedenis van subroutines: https://en.wikipedia.org/wiki/Subroutine
