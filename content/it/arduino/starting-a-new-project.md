---
title:                "Avviare un nuovo progetto"
html_title:           "Arduino: Avviare un nuovo progetto"
simple_title:         "Avviare un nuovo progetto"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Che cos'è e perché?

Iniziare un nuovo progetto con Arduino significa creare un programma che controlla un dispositivo o un sistema, usando una scheda di sviluppo chiamata Arduino. I programmatori lo fanno per sfruttare la flessibilità e la facilità di utilizzo di questo sistema per realizzare le loro idee.

## Come fare:

Ecco un semplice esempio che fa lampeggiare un LED:

```
void setup() {
  pinMode(LED_BUILTIN, OUTPUT); // imposta il LED incorporato come output
}

void loop() {
  digitalWrite(LED_BUILTIN, HIGH); // accende il LED
  delay(1000); // aspetta per 1 secondo
  digitalWrite(LED_BUILTIN, LOW); // spegne il LED
  delay(1000); // aspetta per 1 secondo
}
```

## Un tuffo nel profondo:

Arduino è stato creato nel 2005 da un gruppo di studenti italiani per creare un sistema di prototipazione rapido e a basso costo per i loro progetti. Oggi, ci sono molti altri dispositivi simili, ma Arduino rimane uno dei più popolari per la sua comunità attiva e le sue numerose risorse online. Per iniziare un progetto con Arduino, ti consigliamo di consultare il sito ufficiale di Arduino (https://www.arduino.cc/) e la sua documentazione (https://www.arduino.cc/reference/en/).

## Vedi anche:

- Documentazione di Arduino: https://www.arduino.cc/reference/en/
- Sito ufficiale di Arduino: https://www.arduino.cc/
- ProjectHub, una piattaforma che mostra progetti creati da utenti di Arduino: https://create.arduino.cc/projecthub