---
title:    "Arduino: Få dagens dato"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

# Hvorfor

Mange av dere lurer kanskje på hvorfor man trenger å få tak i nåværende dato i et Arduino-program. Vel, det er faktisk ganske nyttig! Å ha tilgang til riktig dato og klokkeslett kan være avgjørende for å gjøre presisjonsoppgaver som å styre timingen for en automatisk dør eller å planlegge og logge datainnsamling.

# Hvordan

Heldigvis er det enkelt å få tak i nåværende dato på et Arduino-board. Alt du trenger å gjøre er å bruke en "data" type variabel som heter `time_t`. Denne variabelen vil inneholde informasjon om dato og klokkeslett. Så her er en rask kodeeksempel som viser hvordan du kan få tak i nåværende dato og skrive den ut på en serieport:

```
Arduino void setup() {
  Serial.begin(9600); // opprett serieport på 9600 baud
}

void loop() {
  time_t now = time(NULL); // få nåværende dato og klokkeslett
  Serial.println(now); // skriv ut til serieport
  delay(1000); // vent 1 sekund før du gjør det igjen
}
```

Outputet vil se ut som følger:

`1567209600`

Hva betyr dette tallene? Dette er antall sekunder som har gått siden midnatt, 1. januar 1970. Dette er en standard måte å lagre dato og klokkeslett på og det er opp til deg å formatere den på en måte som er nyttig for ditt prosjekt.

# Dypdykk

Det finnes mange ulike biblioteker og funksjoner som kan hjelpe deg med å formatere og bruke datoen på en mer brukervennlig måte. For eksempel kan du bruke `sprintf()` funksjonen for å konvertere `now` variabelen til et leselig format og skrive den ut på en LCD-skjerm.

Det finnes også forskjellige RTC (Real-time Clock) moduler som kan kobles til Arduino-boardet ditt for å få mer nøyaktig tid og dato. Disse modulene bruker ofte en egen batteridrevet klokkechip som sikrer at tid og dato fortsatt holdes selv når Arduino-boardet er slått av.

# Se også

- [How to use a Real Time Clock with Arduino code](https://www.arduino.cc/en/reference/RTC)
- [Arduino Time Library](https://www.pjrc.com/teensy/td_libs_Time.html)
- [Tutorial: Using a DS1307 Real Time Clock with the Arduino](https://learn.adafruit.com/adafruit-arduino-lesson-8-ds1307-real-time-clock-rtc/overview)