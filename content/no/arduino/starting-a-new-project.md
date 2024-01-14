---
title:    "Arduino: Å starte et nytt prosjekt"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/arduino/starting-a-new-project.md"
---

{{< edit_this_page >}}

# Hvorfor

Å starte et nytt prosjekt med Arduino kan være en spennende og givende opplevelse. Det å programmere ulike sensorer og komponenter for å lage et fungerende system kan gi en dypere forståelse for teknologien og åpne opp for kreative muligheter. I tillegg kan det være en morsom måte å lære seg koding på!

# Slik gjør du det

Arduino-programmering er svært tilgjengelig og enkelt å lære seg. Alt du trenger er en Arduino-mikrokontroller, noen sensorer og komponenter, og en datamaskin. Følg disse enkle trinnene for å komme i gang:

```
Arduino.uno

void setup() {
  pinMode(LED_BUILTIN, OUTPUT);
}

void loop() {
  digitalWrite(LED_BUILTIN, HIGH);
  delay(1000);
  digitalWrite(LED_BUILTIN, LOW);
  delay(1000);
}
```

I dette eksempelet bruker vi en Arduino uno og en LED-lysdiode. Først setter vi opp pinnen som LED-en er koblet til som en utgang. Deretter bruker vi en løkke for å skru LED-en av og på med en pause på én sekund mellom hver tilstand. Når du laster opp koden til Arduino-en din, vil du se at LED-en blinker i ett sekund intervaller.

Arduino-plattformen har et nettbasert utviklingsverktøy som heter Arduino Create. Denne plattformen gjør det enda enklere å programmere Arduino ved å tilby en intuitiv brukergrensesnitt og et økosystem av biblioteker og eksempler. Du kan også programmere i tradisjonelle kodespråk som C++ eller Python ved hjelp av Arduino IDE-programvaren.

# Dykk dypere

Når du har fått et grunnleggende grep om Arduino-programmering, kan du begynne å utforske mer avanserte konsepter som variabler, funksjoner og sensorer. Det er også mulig å koble Arduino til internett og kommunisere med andre enheter eller plattformer.

En god ressurs for videre læring er Arduino sin offisielle dokumentasjon og forum. Her finner du tonnevis av nyttige ressurser, fra tutorials og eksempler til diskusjoner med andre Arduino-entusiaster.

# Se også

- [Arduino Norge](https://www.arduino.cc)
- [Arduino Create](https://create.arduino.cc)
- [Arduino Offisiell Dokumentasjon](https://www.arduino.cc/reference/en)
- [Arduino Forum](https://forum.arduino.cc)