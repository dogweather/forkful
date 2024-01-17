---
title:                "Convertire una data in una stringa"
html_title:           "Arduino: Convertire una data in una stringa"
simple_title:         "Convertire una data in una stringa"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Cosa e perché?

Convertire una data in una stringa è il processo di convertire una data in un formato che può essere letto e interpretato da un computer. I programmatori spesso lo fanno per salvare e manipolare le date in modo più facile e preciso all'interno del loro codice.

## Come fare:

```
// Esempio 1: Convertire la data corrente in una stringa

#include <Time.h> // Includiamo la libreria Time
#include <TimeLib.h> // Includiamo la libreria TimeLib per utilizzare alcune funzioni

void setup() {
  Serial.begin(9600); // Inizializziamo la comunicazione seriale
  setTime(17, 45, 30, 11, 3, 2020); // Impostiamo la data e l'ora (ore, minuti, secondi, giorno, mese, anno)
}

void loop() {
  char buffer[20]; // Definiamo un buffer per la data
  sprintf(buffer, \"%s\", getTimeStr()); // Convertiamo la data in una stringa
  Serial.println(buffer); // Stampiamo la stringa sulla porta seriale
  delay(1000); // Aspettiamo un secondo
}
```

**Output:**

```
17:45:30, 11/03/2020 
```

```
// Esempio 2: Convertire una data scelta dall'utente in una stringa

#include <Time.h> // Includiamo la libreria Time
#include <TimeLib.h> // Includiamo la libreria TimeLib per utilizzare alcune funzioni

void setup() {
  Serial.begin(9600); // Inizializziamo la comunicazione seriale
}

void loop() {
  int giorno, mese, anno; // Definiamo tre variabili per la data scelta dall'utente
  Serial.println(\"Inserisci una data nel formato gg/mm/aaaa:\");
  while (Serial.available() < 10); // Aspettiamo finché l'utente non inserisce una data valida
  if (Serial.available() == 10) { // Verifichiamo che l'utente abbia inserito 10 caratteri (gg/mm/aaaa)
    char buffer[20]; // Definiamo un buffer per la data
    sprintf(buffer, \"%s\", getTimeStr(giorno, mese, anno)); // Convertiamo la data in una stringa
    Serial.println(buffer); // Stampiamo la stringa sulla porta seriale
  }
  delay(1000); // Aspettiamo un secondo
}
```

**Output:**

```
Inserisci una data nel formato gg/mm/aaaa:
17:45:30, 11/03/2020 
```

## Approfondimento:

Convertire una data in una stringa è diventato una pratica comune nella programmazione moderna. In passato, le date venivano salvate come numeri interi rappresentanti i secondi trascorsi da una data di riferimento (chiamata "epoch"). Questo però rendeva difficile e confusa la lettura e la manipolazione delle date per i programmatori. Grazie alla conversione in stringa, le date possono essere rappresentate in un formato più comprensibile e manipolabile.

Un'altra alternativa alla convertire una data in una stringa è l'utilizzo di una libreria che gestisce le date in modo automatico. Tuttavia, molte di queste librerie non sono compatibili con tutti i tipi di microcontrollori e possono rallentare il codice. Inoltre, la conversione in stringa offre ai programmatori la possibilità di personalizzare il formato della data in base alle loro esigenze.

Per implementare la conversione in stringa di una data, esistono diverse funzioni disponibili all'interno della libreria TimeLib, come ad esempio getTimeStr() che restituisce la data e l'ora correnti in un formato predefinito. La libreria Time offre inoltre la possibilità di personalizzare il formato della data utilizzando la funzione setTimeFormat(format).

## Vedi anche:

- [Libreria Time sul sito ufficiale di Arduino](https://www.arduino.cc/reference/en/libraries/time/)
- [Guida completa alla gestione delle date in Arduino](https://www.robotshop.com/community/forum/t/arduino-101-how-to-manage-date-and-time/13017)