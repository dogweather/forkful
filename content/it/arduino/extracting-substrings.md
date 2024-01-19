---
title:                "Estrazione di sottosequenze"
html_title:           "Arduino: Estrazione di sottosequenze"
simple_title:         "Estrazione di sottosequenze"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/extracting-substrings.md"
---

{{< edit_this_page >}}

## Che Cosa e Perché?

Estrarre sottostringhe significa prelevare una parte specifica di un testo da un string più grande. I programmatori utilizzano questo metodo per manipolare, analizzare e formattare i dati secondo le necessità del software.

## Come fare:

Ecco qualche esempio utilizzando la funzione `substring()` di Arduino.

```Arduino
String str = "Buongiorno, Italia!";
String sost = str.substring(11, 17);
Serial.println(sost);  // Stampa: Italia
```

Questo codice estrae la sottostringa da una stringa più grande, partendo dall'11° carattere fino al 17° ed infine la stampa.

```Arduino
String str1 = "Pizza Margherita";
String sost1= str1.substring(6);
Serial.println(sost1);  // Stampa: Margherita
```
In questo esempio, la sottostringa viene estratta a partire dal 6° carattere fino alla fine della stringa.

## Approfondimenti

L'uso della estrazione delle sottostringhe ha radici nelle prime fasi dello sviluppo del software, quando i dati testuali venivano elaborati manualmente. 

Un'alternativa alla funzione `substring()` di Arduino potrebbe essere l'uso di `indexOf()` e `charAt()` per ottenere un effetto simile. 

La funzione `substring()` funziona allocando memoria per la nuova sottostringa e copiandoci i caratteri desiderati. Questo dovrebbe essere tenuto in considerazione in termini di gestione della memoria, soprattutto quando si lavora con la piattaforma Arduino rispetto ad altre, come Java, che hanno più risorse di memoria.

## Vedi anche

Per ulteriori informazioni sulla programmazione con Arduino, consulta i seguenti link:

- La documentazione ufficiale della funzione `substring()` di Arduino: [https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/substring/](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/substring/)
- Una guida su come lavorare con le stringhe in Arduino: [https://startingelectronics.org/software/arduino/learn-to-program-course/08-strings/](https://startingelectronics.org/software/arduino/learn-to-program-course/08-strings/)