---
title:                "Arduino: Iniziare un nuovo progetto"
programming_language: "Arduino"
category:             "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Perché

Se sei appassionato di elettronica e programmazione, avrai sicuramente sentito parlare di Arduino. Questo microcontrollore open-source ha fatto sì che chiunque possa realizzare progetti e creare dispositivi interattivi senza dover essere un esperto di programmazione. In questo post, ti spiegheremo come iniziare un nuovo progetto con Arduino e quali risorse utilizzare per aiutarti lungo il percorso.

## Come fare

Per iniziare un nuovo progetto con Arduino, avrai bisogno di alcuni elementi fondamentali: una scheda Arduino (come ad esempio l'Arduino Uno), un cavo USB per collegarlo al computer, un computer con il software Arduino IDE e una buona dose di creatività. Una volta che hai questi elementi a portata di mano, puoi seguire questi semplici passaggi per iniziare:

```Arduino
void setup() {
// Inserisce qui il codice per l'inizializzazione dei componenti
}

void loop() {
// Inserisce qui il codice per il funzionamento del dispositivo
}
```

Il setup() è il primo blocco di codice che viene eseguito quando si accende la scheda Arduino. Qui puoi inizializzare i componenti come sensori, led o display. Nel loop(), invece, inserisci il codice che deve essere eseguito continuamente dal dispositivo.

Ad esempio, se vuoi far accendere un led ogni volta che viene rilevato un movimento dal sensore di movimento, puoi utilizzare questo codice:

```Arduino
int movimentoPin = 2; // Dichiara il pin del sensore come input
int ledPin = 13; // Dichiara il pin del led come output

void setup() {
  pinMode(movimentoPin, INPUT); // Imposta il pin del sensore come input
  pinMode(ledPin, OUTPUT); // Imposta il pin del led come output
}

void loop() {
  if (digitalRead(movimentoPin) == HIGH) { // Se il sensore rileva movimento
    digitalWrite(ledPin, HIGH); // Accendi il led
  } else {
    digitalWrite(ledPin, LOW); // Altrimenti spegni il led
  }
}
```

## Approfondimento

Una delle cose più belle di Arduino è la comunità di appassionati e sviluppatori che si sono creati attorno a questa piattaforma. Ci sono tantissime risorse disponibili online per aiutarti a iniziare un nuovo progetto, come tutorial, forum, e documentazione ufficiale. Inoltre, ci sono diversi kit di sviluppo che includono tutto il necessario per iniziare a sperimentare con Arduino.

Oltre alla componentistica fisica, puoi anche utilizzare l'Arduino IDE per simulare e testare il tuo codice prima di caricarlo sulla scheda. Ci sono anche diverse piattaforme online che ti permettono di creare e condividere progetti con altri utenti.

## Vedi anche

- [Sito ufficiale di Arduino](https://www.arduino.cc/)
- [Canale YouTube di Arduino](https://www.youtube.com/user/arduinoteam)
- [Forum di Arduino](https://forum.arduino.cc/)
- [Scheda di introduzione a Arduino](https://www.tinkercad.com/learn/arduino)
- [Piattaforma di simulazione online di Arduino](https://create.arduino.cc/projecthub)