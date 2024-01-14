---
title:                "Arduino: Ottenere la data corrente"
programming_language: "Arduino"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Perché

Ci sono molte ragioni per cui potresti voler conoscere la data attuale nel tuo progetto Arduino. Forse stai creando un calendario e hai bisogno di visualizzare la data, oppure stai monitorando una pianta e vuoi tenere traccia di quando l'hai annaffiata per l'ultima volta. Qualunque sia il tuo motivo, è importante sapere come ottenere la data corrente per rendere il tuo progetto ancora più efficace.

## Come fare

Per ottenere la data attuale in Arduino, è necessario utilizzare la funzione `now()` della libreria `Time`. Inizialmente, dovrai impostare il tuo orologio in tempo reale (RTC) per garantire che l'Arduino abbia una fonte accurata di tempo. Ci sono molte schede RTC compatibili con Arduino, come la popolare scheda DS1307. Una volta impostato l'RTC, puoi utilizzare il seguente codice per visualizzare la data attuale:

```arduino
#include <Time.h> // inclusione della libreria Time
void setup() {
  Serial.begin(9600); // inizializza la comunicazione seriale
}
void loop() {
  time_t t = now(); // memorizza la data e l'ora correnti nella variabile t
  Serial.print("La data attuale è: ");
  Serial.print(year(t)); // stampa l'anno corrente
  Serial.print("-");
  Serial.print(month(t)); // stampa il mese corrente
  Serial.print("-");
  Serial.println(day(t)); // stampa il giorno corrente
  delay(1000); //Aspetta un secondo prima di ripetere
}
```

Una volta caricato questo codice nella tua scheda Arduino, puoi aprire il monitor seriale per visualizzare la data attuale stampata ogni secondo. Puoi anche personalizzare l'output in base alle tue esigenze, ad esempio aggiungendo le informazioni sull'ora e i secondi.

## Approfondimento

Se desideri approfondire ulteriormente il concetto di data attuale in Arduino, puoi esplorare le varie funzioni della libreria Time. Ad esempio, puoi utilizzare la funzione `hour()` per ottenere l'ora corrente o `weekday()` per ottenere il giorno della settimana. Puoi anche utilizzare la funzione `now()` per impostare una variabile con la data e l'ora attuali e utilizzarla successivamente nel tuo codice.

Inoltre, se vuoi utilizzare la data per scopi più avanzati, puoi anche combinare la libreria Time con altre librerie come la libreria SD per registrare le date e i tempi delle tue misurazioni o la libreria Wireless per inviare dati con la data corrente tramite connessione Wi-Fi.

## Vedi anche

- [Libreria RTCtime](https://www.arduino.cc/reference/en/libraries/rtctime/)
- [Libreria Time](https://www.arduino.cc/reference/en/libraries/time/)
- [Tutorial su come impostare un RTC e utilizzare la libreria Time](https://randomnerdtutorials.com/arduino-ds1307-rtc-with-at24c32-eeprom/
- [Tutorial su come utilizzare la libreria Time in combinazione con la libreria SD](https://diyi0t.com/arduino-sd-card-data-logger/)