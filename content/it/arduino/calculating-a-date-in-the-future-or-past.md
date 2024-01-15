---
title:                "Calcolare una data nel futuro o nel passato"
html_title:           "Arduino: Calcolare una data nel futuro o nel passato"
simple_title:         "Calcolare una data nel futuro o nel passato"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Perché

Hai mai desiderato sapere in che giorno della settimana cadrà il tuo compleanno fra 5 anni? O forse hai bisogno di calcolare una data futura per un progetto? Con l'Arduino, puoi facilmente scrivere un programma per calcolare le date in avanti o all'indietro.

## Come fare

Per calcolare una data futura o passata con Arduino, segui questi semplici passaggi:

1. Importa la libreria Time.h inserendo `#include <Time.h>` all'inizio del tuo codice.
2. Definisci le variabili per la data e l'ora utilizzando il tipo `tmElements_t`. Ad esempio, `tmElements_t futureDate`.
3. Usa la funzione `breakTime` per decomporre la data in variabili individuali. Ad esempio, `breakTime(currentTime, futureDate)`.
4. Modifica le variabili desiderate, come ad esempio l'anno, il mese o il giorno, per ottenere la data desiderata. Ad esempio, `futureDate.Year = 2026`.
5. Utilizza la funzione `makeTime` per ricomporre la data e ottenere il timestamp. Ad esempio, `unsigned long futureTime = makeTime(futureDate)`.

Ecco un esempio di come potrebbe apparire il tuo codice:

```Arduino
#include <Time.h>

tmElements_t futureDate; // Definisci la variabile per la data
unsigned long futureTime; // Definisci la variabile per il timestamp

void setup() {
  setSyncProvider(RTC.get); // Imposta il provider per il sincronizzazione dell'orologio in tempo reale
  setTime(12, 0, 0, 1, 1, 2021); // Imposta la data attuale
  breakTime(now(), futureDate); // Decomponi la data attuale in variabili
  futureDate.Year = 2026; // Modifica l'anno per ottenere la data futura desiderata
  futureTime = makeTime(futureDate); // Ricomponi la data e ottieni il timestamp
}

void loop() {
  // Fai qualcosa con il timestamp, come ad esempio visualizzarlo su un display
  delay(1000); // Attendi un secondo
}
```

## Approfondimento

Se sei interessato a saperne di più su come funzionano i calcoli di date con l'Arduino, ecco alcune informazioni utili:

- In Arduino, le date e le ore sono rappresentate come timestamp, ovvero un numero intero che indica il numero di secondi trascorsi dal 1 gennaio 1970. Questo è conosciuto come "epoch time".
- Utilizzando la libreria Time.h, puoi facilmente convertire un timestamp in variabili di date e ore, e viceversa.
- Assicurati di impostare correttamente la data e l'ora sul tuo Arduino prima di utilizzare le funzioni `now()` e `makeTime()`.
- Controlla sempre i limiti delle variabili, ad esempio l'anno deve essere compreso tra 1970 e 2106.

## Vedi anche

- [Tutorial su come utilizzare la libreria Time.h con Arduino](https://www.arduino.cc/en/Tutorial/TimeLibrary)
- [Documentazione ufficiale della libreria Time.h](https://www.pjrc.com/teensy/td_libs_Time.html)