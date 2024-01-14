---
title:                "Arduino: Eliminare i caratteri corrispondenti a un modello"
simple_title:         "Eliminare i caratteri corrispondenti a un modello"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Perché

Se sei un appassionato di Arduino e hai familiarità con la programmazione, potresti aver sentito parlare della possibilità di eliminare i caratteri che corrispondono a un determinato pattern. Ma perché dovresti farlo? Ci sono molti casi in cui questa funzionalità può risultare utile. Ad esempio, può aiutare a ottenere un'input più pulito da un sensore o a rimuovere caratteri non desiderati da una stringa di dati.

## Come Farlo

Per eliminare i caratteri che corrispondono a un certo pattern in Arduino, è possibile utilizzare la funzione `replace()`. Questa funzione prende tre argomenti: la stringa da modificare, il carattere da eliminare e il carattere con cui sostituirlo. Di seguito è riportato un esempio di come utilizzare questa funzione:

```
Arduino
// Creiamo una stringa di esempio
String str = "ciao mondo!";

// Utilizziamo la funzione replace per eliminare il carattere "o" e sostituirlo con un carattere vuoto
str.replace("o", "");

// Stampiamo la nuova stringa
Serial.println(str); // cia mondo!
```

È importante notare che la funzione `replace()` modifica la stringa originale. Se si desidera invece creare una nuova stringa senza i caratteri corrispondenti al pattern, è possibile utilizzare la funzione `substring()` per estrarre solo i caratteri desiderati dalla stringa originale.

## Approfondimento

Per coloro che vogliono esplorare ulteriormente la possibilità di eliminare caratteri che corrispondono a un pattern in Arduino, ci sono molte altre funzioni utili da esplorare. Ad esempio, la funzione `remove()` permette di eliminare una porzione specifica di una stringa, mentre la funzione `indexOf()` restituisce l'indice della prima occorrenza di un carattere o di un determinato pattern. Sperimentare con queste funzioni può aiutare a comprendere meglio come gestire e manipolare le stringhe in Arduino.

## Vedi Anche

- [Documentazione ufficiale delle stringhe in Arduino](https://www.arduino.cc/reference/en/language/variables/data-types/string/)
- [Tutorial su come manipolare le stringhe in Arduino](https://www.arduino.cc/en/Tutorial/String)
- [Esempi di codice per la gestione delle stringhe in Arduino](https://www.tutorialspoint.com/arduino/arduino_strings.htm)