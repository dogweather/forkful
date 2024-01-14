---
title:                "Arduino: Å starte et nytt prosjekt"
programming_language: "Arduino"
category:             "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/starting-a-new-project.md"
---

{{< edit_this_page >}}

# Hvorfor?

Å starte et nytt prosjekt med Arduino kan være en spennende og givende opplevelse. Med Arduino kan du enkelt lage dine egne elektroniske apparater og roboter. Det er også en flott måte å lære om programmering og elektronikk på, samtidig som du kan skape noe som faktisk fungerer.

# Hvordan?

For å starte et Arduino-prosjekt trenger du selvfølgelig en Arduino-enhet, enten det er en Uno, Nano eller annen modell. Du trenger også noen grunnleggende deler som motstander, LED-lys, jumperkabler og en breadboard.

Først må du koble Arduinoen til datamaskinen din og laste ned den nyeste versjonen av Arduino-programvaren. Deretter kan du begynne å skrive koden din i det åpne tekstfeltet i programvaren. Her er et eksempel på en enkel kode som får en LED til å blinke:

```Arduino
// Sett opp pinne 13 som utgang
void setup() {
  pinMode(13, OUTPUT);
}

// Gjenta for alltid
void loop() {
  // Skru på LED-en
  digitalWrite(13, HIGH);
  // Vent i ett sekund
  delay(1000);
  // Skru av LED-en
  digitalWrite(13, LOW);
  // Vent i ett sekund
  delay(1000);
}
```

Når du har skrevet koden din og lagret den, kan du laste den opp til Arduinoen ved å klikke på "Upload" -knappen. Du vil da se at LED-lyset blinker i ett sekund av gangen.

# Dykk dypere

Det er utallige muligheter og muligheter når du starter et Arduino-prosjekt. Du kan leke deg med sensorer og aktuatorer, lage interaktive prosjekter med knapper og LCD-skjermer, eller til og med kommunisere med andre enheter via Bluetooth eller Wi-Fi.

Det er også viktig å nevne at Arduino-miljøet er stort og aktivt, med mange ressurser og guider tilgjengelig online. Det er også vanligvis lett å finne lignende prosjekter som andre har gjort for inspirasjon og hjelp.

Så hva venter du på? Koble til Arduinoen din og begynn å utforske de endeløse mulighetene som ligger foran deg.

# Se også

- Offisiell Arduino-nettside: https://www.arduino.cc/
- Arduino-prosjekter og guider: https://www.instructables.com/arduino/
- Arduino-fellesskapet og forum: https://forum.arduino.cc/