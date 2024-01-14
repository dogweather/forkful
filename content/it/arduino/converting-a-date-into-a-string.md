---
title:                "Arduino: Convertire una data in una stringa"
simple_title:         "Convertire una data in una stringa"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Perché

Convertingire una data in formato stringa è un'operazione utile per visualizzare informazioni su un display o per salvare dati in un file di testo. Questo può essere particolarmente utile quando si lavora con dati temporali in un progetto Arduino.

## Come Fare

Il primo passo per convertire una data in una stringa è utilizzare la funzione `String()` per creare un oggetto stringa vuoto. Dopodiché, è necessario utilizzare le funzioni `hour()`, `minute()`, `second()` e `day()`, `month()`, `year()` per estrarre le informazioni dalla data desiderata. Infine, queste informazioni possono essere concatenate all'oggetto stringa utilizzando l'operatore `+`.

```Arduino 
// Inizializza l'oggetto stringa
String data = "";

// Estrapola informazioni dalla data
int ora = hour();
int minuti = minute();
int secondi = second();
int giorno = day();
int mese = month();
int anno = year();

// Concatena le informazioni all'oggetto stringa
data = data + ora + ":" + minuti + ":" + secondi + " " + giorno + "/" + mese + "/" + anno;

// Stampa la data in formato stringa
Serial.println(data);
```

Output: `14:25:03 22/07/2021`

## Approfondimento

Esistono anche diverse librerie disponibili per semplificare la conversione di una data in una stringa su Arduino. Ad esempio, la libreria `TimeLib` permette di utilizzare la funzione `now()` per ottenere un oggetto `tmElements_t` contenente le informazioni sulla data e utilizzarle per creare una stringa con la funzione `makeDateString()`.

```Arduino 
// Includi la libreria TimeLib
#include "TimeLib.h"

// Inizializza l'oggetto tmElements_t
tmElements_t data;

// Imposta la data desiderata
data.Hour = 14;
data.Minute = 25;
data.Second = 3;
data.Day = 22;
data.Month = 07;
data.Year = 2021 - 1970; // Il valore del parametro Year deve essere espresso come numero di anni dal 1970

// Converti la data in una stringa e stampala
String dataString = makeDateString(data);
Serial.println(dataString);
```

Output: `14:25:03 22/07/2021`

È importante notare che entrambi gli esempi sopra utilizzano il formato di data americano MM/DD/YYYY. Se si desidera utilizzare un formato differente, è possibile utilizzare una variabile di tipo `char` per specificare un diverso ordine di concatenazione delle informazioni.

## Vedi Anche
- Guida ad Arduino
- [Documentazione su `String()`](https://www.arduino.cc/reference/en/language/variables/data-types/string/)
- [Documentazione su `TimeLib.h`](https://github.com/PaulStoffregen/Time)
- [Tutorial su come utilizzare l'oggetto `tmElements_t`](https://www.theengineeringprojects.com/2017/03/introduction-to-tmelements_t-structure-in-arduino-dtmf-controlled-home-automation.html)
- [Altro esempio di conversione di una data in una stringa su Arduino](https://www.techwalla.com/articles/how-to-convert-a-date-to-string-in-arduino)