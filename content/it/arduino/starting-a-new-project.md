---
title:                "Iniziare un nuovo progetto"
html_title:           "Arduino: Iniziare un nuovo progetto"
simple_title:         "Iniziare un nuovo progetto"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Cos'è & Perché?
Iniziare un nuovo progetto in Arduino significa crearne uno nuovo da zero, con l'obiettivo di risolvere un problema specifico o di lavorare su una idea particolare. I programmatori fanno ciò per creare soluzioni uniche, apprendere nuove competenze o per l'excitement di creare qualcosa di veramente personale.

## Come Fare:
Iniziare un nuovo progetto è semplice con la seguente struttura:
```Arduino
void setup() {
  // Inizializza le variabili qui
}

void loop() {
  // Codice da eseguire ripetutamente qui
}
```
Esempio di un blink LED sull’uscita digitale 13:
```Arduino
void setup() {
  pinMode(13, OUTPUT);
}

void loop() {
  digitalWrite(13, HIGH);
  delay(1000);
  digitalWrite(13, LOW);
  delay(1000);
}
```
Risultato atteso: il LED lampeggia ogni secondo.

## Approfondimento
Arduino nacque nel 2005 come progetto di tesi di Massimo Banzi, studente presso l'Interaction Design Institute Ivrea. L'obiettivo principale era creare uno strumento semplice ed accessibile per studenti non ingegneri. L'Arduino ha molti "fratelli", tra cui Raspberry Pi e BeagleBone. Tuttavia, Arduino si distingue per la sua semplicità e facilità d'uso, rendendolo ideale per i principianti. 

Per iniziare un nuovo progetto, si può utilizzare l'IDE (Integrated Development Environment) Arduino. L'IDE Arduino contiene una libreria standard chiamata "Wire" che gestisce le communicazioni I2C, rendendo molto semplice interfacciarsi con la maggior parte dei sensori ed attuatori.

## Vedere Anche
1. [Sito ufficiale di Arduino](https://www.arduino.cc/)
2. [Arduino Language Reference](https://www.arduino.cc/reference/en/)
3. [Progetti Arduino su Instructables](https://www.instructables.com/topics/?sort=none&inChannel=technology&inTopic=arduino)
4. [Forum Arduino](https://forum.arduino.cc/)
5. [Arduino Playground](https://playground.arduino.cc/)