---
title:                "Arduino: Confronto tra due date"
programming_language: "Arduino"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/comparing-two-dates.md"
---

{{< edit_this_page >}}

##Perché
C'è un motivo comune per comparare due date quando si programma con Arduino, ad esempio per controllare se un evento è avvenuto prima o dopo una data specifica. In questo articolo imparerai come fare questo confronto in modo efficiente utilizzando il linguaggio di programmazione di Arduino.

##Come Fare
Per confrontare due date con Arduino, è necessario prima impostare le date come variabili. Ad esempio, si può utilizzare la funzione di data e ora integrata di Arduino per impostare la data come variabile.

```Arduino
#include <TimeLib.h>
int giorno = 28;
int mese = 9;
int anno = 2021;
```

Una volta impostate le date come variabili, è possibile utilizzare l'operatore Booleano "maggiore di" e "minore di" per confrontare le date. Ad esempio, se si vuole controllare se la data è successiva al 28 Settembre 2021, si può utilizzare il seguente codice:

```Arduino
if (giorno > 28 && mese == 9 && anno == 2021) {
  // Codice da eseguire se la condizione è vera
}
```

È importante notare che l'utilizzo dell'operatore Booleano "uguale a" è necessario per confrontare le variabili dei mesi e degli anni.

##Deep Dive
È possibile andare più in profondità nel confronto di due date utilizzando la struttura di dati "struct" di Arduino. Questo permette di creare una variabile che combina giorno, mese e anno in una sola variabile, rendendo il confronto più semplice.

```Arduino
struct Data {
  int giorno;
  int mese;
  int anno;
};

Data data1 = {28, 9, 2021};
Data data2 = {30, 9, 2021};

if (data2.giorno > data1.giorno && data2.mese == data1.mese && data2.anno == data1.anno) {
  // Codice da eseguire se la data2 è successiva alla data1
}
```

##Vedi Anche
- [Cosa è Arduino?](https://it.wikipedia.org/wiki/Arduino)
- [Documentazione di TimeLib](https://www.arduino.cc/reference/en/libraries/timelib/)
- [Approfondimenti su utilizzo delle strutture di dati in Arduino](https://www.programming-electronics-diy.xyz/2017/10/explaining-struct-in-arduino.html)

Buona programmazione con Arduino!