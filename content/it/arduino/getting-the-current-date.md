---
title:    "Arduino: Ottenere la data corrente"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

# Perché

Molti progetti basati su Arduino richiedono di avere la data e l'ora correnti come parte del loro funzionamento. Questa informazione è fondamentale per sincronizzare orologi, programmare eventi o semplicemente tenere traccia del tempo trascorso.

# Come fare

Per ottenere la data e l'ora correnti su Arduino, ci sono alcune cose da considerare. In primo luogo, è necessario avere un modulo di tempo esterno collegato alla scheda Arduino. Ci sono diversi tipi di moduli disponibili sul mercato, ma i più comuni sono quelli basati sul chip DS1307 o DS3231.

Una volta collegato il modulo di tempo alla scheda, è possibile utilizzare la libreria Wire per comunicare con esso tramite il bus I2C. Per utilizzare questa libreria, è necessario includerla all'inizio del tuo codice:

```Arduino
#include <Wire.h>
```

A questo punto, è possibile utilizzare le diverse funzioni offerte dalla libreria per comunicare con il modulo di tempo e ottenere la data e l'ora correnti. Ad esempio, la funzione `Wire.begin()` deve essere chiamata all'inizio del tuo codice per aprire una connessione al bus I2C.

Ora, per ottenere la data e l'ora correnti, possiamo utilizzare la funzione `Wire.requestFrom()` per inviare una richiesta al modulo di tempo e leggere i dati restituiti. Ecco un esempio di codice per ottenere la data corrente:

```Arduino
#include <Wire.h>

void setup() {
  // Inizializza la connessione I2C
  Wire.begin();
  Serial.begin(9600);
}

void loop() {
  // Richiede 7 byte dal modulo di tempo
  Wire.requestFrom(0x68, 7);
  // Leggi i dati e assegnali alle variabili necessarie
  int seconds = Wire.read();
  int minutes = Wire.read();
  int hours = Wire.read();
  int day = Wire.read();
  int month = Wire.read();
  int year = Wire.read();
  // Stampa la data corrente
  Serial.print(hours); Serial.print(":");
  Serial.print(minutes); Serial.print(":");
  Serial.println(seconds);  
}
```

Ecco un esempio di output che potresti ottenere:

```
7:10:30
```

Oltre a ottenere l'ora, è possibile utilizzare lo stesso sistema per ottenere anche la data e altri dati relativi al tempo. Assicurati di controllare la documentazione della tua libreria e del tuo modulo di tempo per ulteriori informazioni sulle funzioni disponibili e sul loro utilizzo corretto.

# Approfondimento

Ovviamente, l'uso di un modulo di tempo esterno è solo una delle tante opzioni disponibili per ottenere la data e l'ora correnti su Arduino. Alcune schede Arduino, come l'Arduino Uno, hanno un orologio interno integrato che può essere utilizzato senza bisogno di un modulo esterno.

Inoltre, esistono anche diverse librerie disponibili online che offrono funzionalità più avanzate per la gestione del tempo e la sincronizzazione con server NTP (Network Time Protocol) per ottenere la data e l'ora precise.

# Vedi anche

- [Documentazione ufficiale di Arduino Wire library](https://www.arduino.cc/en/reference/wire)
- [Tutorial su collegamento e utilizzo di un modulo DS1307](https://www.arduino.cc/en/Tutorial/RTClib)
- [Esempio di utilizzo di un orologio interno con Arduino](https://create.arduino.cc/projecthub/pibee/an-alarm-clock-with-arduino-619ccb)