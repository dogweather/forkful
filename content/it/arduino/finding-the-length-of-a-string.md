---
title:                "Trovare la lunghezza di una stringa"
html_title:           "Arduino: Trovare la lunghezza di una stringa"
simple_title:         "Trovare la lunghezza di una stringa"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Che Cos'è & Perché?
La ricerca della lunghezza di una stringa è il processo di conteggio del numero totale di caratteri presenti in una sequenza di caratteri specifica. I programmatori eseguono questa operazione per allocare correttamente la memoria, per validare l'input dell'utente o per eseguire operazioni di manipolazione delle stringhe.

## Come Fare:
Questo è un esempio di come trovare la lunghezza di una stringa in Arduino utilizzando la funzione `strlen()`:

```Arduino
char frase[] = "Buongiorno";
Serial.begin(9600);
Serial.println(strlen(frase));
```
L'output sarà `10`, che è il numero totale di caratteri presenti nella stringa "Buongiorno".

## Approfondimenti:
1. **Contesto storico**: La funzione `strlen()` fu introdotta per la prima volta nel linguaggio di programmazione C e poi ereditata da Arduino. 
2. **Alternative**: Sebbene `strlen()` sia la funzione più comune per trovare la lunghezza di una stringa, esistono anche altre funzioni come `size()` e `length()` nel caso di oggetti di stringa.
3. **Dettagli di implementazione**: La funzione `strlen()` calcola la lunghezza di una stringa iterando sui caratteri fino a raggiungere il terminatore nullo (`\0`). 

## Vedi Anche:
Per ulteriori informazioni e un maggiore approfondimento, potete consultare queste risorse:


- [Arduino String Reference](https://www.arduino.cc/reference/en/language/variables/data-types/string/)
- [Intro to Arduino String Manipulation](https://create.arduino.cc/projecthub/muhammad_aqib/arduino-string-manipulation-using-minimal-code-features-94b275)
- [Arduino String Functions](https://www.tutorialspoint.com/arduino/arduino_strings.htm)