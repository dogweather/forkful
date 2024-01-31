---
title:                "Organisering av kode i funksjoner"
date:                  2024-01-26T01:09:02.745339-07:00
model:                 gpt-4-1106-preview
simple_title:         "Organisering av kode i funksjoner"

category:             "Arduino"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å organisere koden i funksjoner betyr å dele opp koden din i gjenbrukbare biter, hvor hver bit utfører en spesifikk jobb. Programmerere gjør dette for å gjøre koden enklere å lese, feilsøke og gjenbruke. Det er som å sortere Lego i bokser - det sparer deg fra å rote gjennom en kaotisk haug hver gang du vil bygge noe.

## Hvordan:
Tenk at du vil få en LED til å blinke. Uten funksjoner, er `loop`-en din en rotete klump. Med funksjoner, er den ryddig. Her er hvordan:

```Arduino
const int LED_PIN = 13;

void setup() {
  pinMode(LED_PIN, OUTPUT);
}

void loop() {
  blinkLED(500); // Blink LED-en hver 500ms
}

// Funksjon for å få en LED til å blinke
void blinkLED(int forsinkelsestid) {
  digitalWrite(LED_PIN, HIGH);
  delay(forsinkelsestid);
  digitalWrite(LED_PIN, LOW);
  delay(forsinkelsestid);
}
```

Eksempel på output: LED-en din blinker lystig, og koden formål er klar ved første øyekast.

## Dypdykk
Før funksjoner var programmering en lineær biltur; du så hvert eneste hull fra start til slutt. Etter funksjoner, er det mer som å hoppe på flyvninger - du hopper over til de viktige delene. Historisk sett var underprogrammer (tidlige funksjoner) en revolusjon i programmering, som lot kodere unngå å gjenta seg selv – det er DRY-prinsippet, Don't Repeat Yourself (Ikke gjenta deg selv). Alternativer til funksjoner kan inkludere makroer eller bruk av klasser for objektorientert programmering (OOP). Detaljene? Når du definerer en funksjon, gir du kompilatoren en blåkopi for utførelsen av en oppgave. Med Arduino definerer du ofte void-funksjoner som fungerer som enkle kommandoer for en mikrokontroller, men funksjoner kan også returnere verdier, noe som gjør dem mer allsidige.

## Se Også
For mer om funksjoner, ta en titt på disse:

- Arduinos offisielle referanse for funksjoner: https://www.arduino.cc/reference/en/language/functions/
- Lær mer om DRY-prinsippet: https://en.wikipedia.org/wiki/Don%27t_repeat_yourself
- En oppfriskning på historien om underprogrammer: https://en.wikipedia.org/wiki/Subroutine
