---
title:    "Arduino: Convertire una data in una stringa"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Perché

Convertire una data in un formato testuale è spesso necessario quando si vogliono mostrare le informazioni temporali in modo leggibile per gli utenti. Inoltre, può essere utile per il salvataggio dei dati o per la comunicazione con altri dispositivi.

## Come fare

Per convertire una data in una stringa su Arduino, è possibile utilizzare la funzione `itoa()`, che converte un numero in una stringa di caratteri. Qui di seguito è un esempio di codice che mostra come utilizzare questa funzione per convertire la data corrente in una stringa:

```Arduino
#include <RTClib.h> //libreria per il modulo RTC
RTC_DS1307 rtc; //oggetto della classe RTC_DS1307
DateTime now; //oggetto per la data corrente
char buffer[20]; //buffer per la stringa di output

void setup() {
  rtc.begin(); //inizializza il modulo RTC
  now = rtc.now(); //assegna la data corrente all'oggetto "now"
  itoa(now.day(), buffer, 10); //converte il giorno in una stringa
  Serial.println(buffer); //stampa il giorno su seriale
}

void loop() {
  //nessuna attività in loop
}
```

L'output sarà il giorno corrente su seriale.

## Approfondimento

Ci sono diverse librerie disponibili per facilitare la conversione delle date in stringhe su Arduino, tra cui la libreria `DateTime` utilizzata nell'esempio precedente. Inoltre, è possibile personalizzare il formato della data in base alle esigenze del progetto, aggiungendo ad esempio il nome del mese o l'ora.

## Vedi anche

- [Libreria DateTime per RTC su Arduino](https://playground.arduino.cc/Code/DateTime/)
- [Convertire un numero in una stringa su Arduino](https://www.arduino.cc/reference/en/language/functions/communication/serial/itoa/)
- [Esempio di formattazione personalizzata della data con Arduino](https://www.arduinolibraries.info/libraries/date-format)