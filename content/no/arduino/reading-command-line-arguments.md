---
title:                "Lese kommandolinjeargumenter"
html_title:           "Arduino: Lese kommandolinjeargumenter"
simple_title:         "Lese kommandolinjeargumenter"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Kommandolinje-argumenter tillater input når du kjører koden. Dette gir programmerere fleksibilitet til å tilpasse koden uten kontinuerlige endringer.

## Slik gjør du:

Arduino støtter ikke direkte kommandolinje-argumenter på grunn av sin innebygde natur. Men du kan sende data gjennom seriell kommunikasjon. Her er et eksempel:

```Arduino
void setup() {
  Serial.begin(9600);
}

void loop() {
  if (Serial.available() > 0) {
    int input = Serial.parseInt();
    Serial.println(input);
  }
}
```

Kjører du denne koden og sender '123' gjennom Serial Monitor, får du '123' i retur.

## Dyp Dykk

Kommandolinje-argumenter har vært en del av programmering siden tidlig C. Med Arduino, kan du ikke bruke dem direkte, men alternativene inkluderer bruk av Serial-kommunikasjon eller HTTP-forespørsler for nettverkskoblede enheter.

Implementasjonen innebærer å sjekke tilgjengeligheten av input, lese den, og bruke den som ønsket. Husk at Arduino vil fortsette å lese input som en streng til den mottar en ikke-numerisk verdi.

## Se Også

For mer informasjon om Serial-kommunikasjon i Arduino, sjekk ut:
[Arduino Serial Dokumentasjon](https://www.arduino.cc/reference/en/language/functions/communication/serial/)

For mer om styring av Arduino over nettverk:
[Arduino Ethernet Shield](https://store.arduino.cc/usa/arduino-ethernet-shield-2)