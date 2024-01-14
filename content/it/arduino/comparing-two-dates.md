---
title:    "Arduino: Confrontare due date"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Perché

Comparare le date è un'operazione comune nei progetti di Arduino. Questo può essere utile per determinare il tempo trascorso tra due eventi o per eseguire azioni in determinati giorni o orari. Imparare a confrontare due date ti permetterà di creare progetti più sofisticati e flessibili con il tuo Arduino.

## Come Fare

Per confrontare due date in Arduino, possiamo utilizzare la libreria Time.h. Prima di tutto, è necessario impostare la data corrente e la data di confronto come oggetti della classe tm, come mostrato di seguito:

```Arduino
#include <Time.h>

// Imposta la data corrente come oggetto tm
tm currentDate = {0, 0, 0, 5, 12, 2021};

// Imposta la data di confronto come oggetto tm
tm compareDate = {0, 0, 0, 1, 1, 2020};
```

Una volta impostate le date, possiamo utilizzare la funzione `makeTime()` per convertirle in secondi. Quindi, possiamo confrontare i due valori per determinare quale data è successiva. Ad esempio, se vogliamo confrontare il numero di secondi tra le due date, possiamo utilizzare questo codice di esempio:

```Arduino
// Converte le date in secondi
time_t currentDateInSeconds = makeTime(currentDate);
time_t compareDateInSeconds = makeTime(compareDate);

// Confronta i due valori
if (currentDateInSeconds > compareDateInSeconds) {
  Serial.println("La data corrente è successiva alla data di confronto.");
} else if (currentDateInSeconds < compareDateInSeconds) {
  Serial.println("La data di confronto è successiva alla data corrente.");
} else {
  Serial.println("Le due date sono uguali.");
}
```

L'output di questo codice dipenderà dalla data impostata. Ad esempio, se la data corrente è successiva alla data di confronto, l'output sarà: "La data corrente è successiva alla data di confronto."

## Approfondimenti

Esistono diverse opzioni per confrontare due date in Arduino, come ad esempio l'utilizzo della funzione `year()`, `month()` e `day()`, che restituiscono il valore di un determinato elemento della data. Puoi esplorare ulteriormente queste opzioni e sperimentare con diversi esempi per comprendere meglio il concetto di confronto tra date in Arduino.

## Vedi Anche

- [Documentazione della libreria Time.h](https://www.arduino.cc/en/reference/time)
- [Esempi di confronto tra date in Arduino](https://www.arduino.cc/en/Tutorial/libraryExamples)
- [Tutorial su come utilizzare la libreria Time.h](https://lastminuteengineers.com/using-ds3231-rtc-with-arduino-tutorial/)