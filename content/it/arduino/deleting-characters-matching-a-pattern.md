---
title:                "Eliminazione dei caratteri corrispondenti a un modello"
html_title:           "Arduino: Eliminazione dei caratteri corrispondenti a un modello"
simple_title:         "Eliminazione dei caratteri corrispondenti a un modello"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Perché

Spesso, quando si lavora con una stringa di testo su Arduino, può essere necessario eliminare determinati caratteri che corrispondono ad un certo modello o pattern. Questo può essere utile, ad esempio, per pulire una stringa prima di utilizzarla in un'altra parte del codice.

## Come farlo

Ecco un esempio di come eliminare i caratteri che corrispondono ad un determinato pattern utilizzando la funzione `replace()`:

```Arduino
String testo = "La mia stringa di testo!";
// il carattere di spazio vuoto sarà eliminato
testo.replace(" ", "");
Serial.println(testo); // output: Lamiastringaditesto!
```

In questo caso, il carattere di spazio vuoto è stato eliminato dalla stringa `testo` utilizzando la funzione `replace()`. Si possono anche utilizzare altri caratteri o anche parole intere per eliminare determinate parti della stringa.

## Approfondimento

Oltre alla funzione `replace()`, esistono altre opzioni per eliminare caratteri da una stringa su Arduino. Ad esempio, si può utilizzare la funzione `remove()` che permette di eliminare un singolo carattere o una sottostringa specifica. Inoltre, è possibile utilizzare la libreria `string.h` per utilizzare funzioni come `strtok()` o `strstr()` per manipolare le stringhe.

## Vedi anche

- Documentazione ufficiale di Arduino su `replace()`: https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/replace/
- Tutorial su come manipolare stringhe su Arduino: https://www.arduinolibraries.info/libraries/string
- Esempi di utilizzo della libreria `string.h` su Arduino: https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/