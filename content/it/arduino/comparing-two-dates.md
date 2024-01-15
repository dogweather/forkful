---
title:                "Confronto di due date"
html_title:           "Arduino: Confronto di due date"
simple_title:         "Confronto di due date"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Perché
Ci sono molte situazioni in cui è necessario confrontare due date, ad esempio per controllare se è scaduto un determinato periodo di tempo o per verificare se una data è successiva a un'altra. In questo articolo impareremo come confrontare due date utilizzando Arduino.

## Come fare
Per confrontare due date in Arduino, è necessario utilizzare la libreria Time, che permette di gestire il tempo e la data. Per prima cosa, inseriamo l'import della libreria all'inizio del nostro codice:
```Arduino
#include <Time.h>
```
Successivamente, dobbiamo definire due variabili di tipo Time per le nostre due date da confrontare:
```Arduino
Time data1;
Time data2;
```
Per assegnare una data alle variabili, possiamo utilizzare la funzione `setTime()` specificando il giorno, il mese, l'anno, l'ora e i minuti:
```Arduino
data1.setTime(giorno, mese, anno, ora, minuti);
data2.setTime(giorno, mese, anno, ora, minuti);
```
Una volta assegnati i valori alle variabili, possiamo utilizzare l'operatore `>` per verificare se una data è successiva all'altra. Ad esempio, se vogliamo verificare se `data1` è successiva a `data2`, possiamo utilizzare il seguente codice:
```Arduino
if (data1 > data2) {
  // eseguire qualcosa se la data1 è successiva a data2
}
```
In caso contrario, possiamo utilizzare l'operatore `<` per verificare se `data1` è precedente a `data2`:
```Arduino
if (data1 < data2) {
  // eseguire qualcosa se la data1 è precedente a data2
}
```
Per altre operazioni di confronto, possiamo utilizzare gli operatori `==` (uguaglianza), `>=` (maggiore o uguale) e `<=` (minore o uguale).

## Approfondimenti
Nella libreria Time sono disponibili anche altre funzioni utili per lavorare con le date, come `month(time)`, `year(time)` e `day(time)` per ottenere il mese, l'anno e il giorno di una data specifica. Per ulteriori informazioni e dettagli sulle funzioni disponibili, è possibile consultare la documentazione ufficiale della libreria Time.

## Vedi anche
- [Documentazione ufficiale della libreria Time](https://www.arduino.cc/en/reference/time)
- [Come lavorare con le date in Arduino](https://www.arduino.cc/en/Tutorial/Time)