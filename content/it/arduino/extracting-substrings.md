---
title:                "Estrazione di sottostringhe"
html_title:           "Arduino: Estrazione di sottostringhe"
simple_title:         "Estrazione di sottostringhe"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/extracting-substrings.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Estrarre sottostringhe significa ottenere una parte di una stringa più grande. I programmatori fanno questo per ottenere dati specifici da una stringa o per manipolare i dati in modo più efficiente.

## Come fare:
La funzione `substring()` in Arduino può essere utilizzata per estrarre una sottostringa da una stringa più grande. Di seguito è riportato un esempio di codice che estrae la seconda e la terza lettera da una stringa:
```
Arduino String testo = "Ciao mondo!";
String sottostringa = testo.substring(1, 3);
Serial.println(sottostringa); // Stampa "ia"
```

## Approfondimento:
La funzione `substring()` è stata originariamente introdotta nell'editor di testo Emacs negli anni '70 ed è diventata una caratteristica comune in molti linguaggi di programmazione. Tuttavia, alcune alternative ad essa includono l'utilizzo di espressioni regolari o di funzioni di manipolazione delle stringhe con una logica personalizzata. L'implementazione della funzione `substring()` in Arduino utilizza la logica di copia dei caratteri da una posizione di indice specificata a un'altra.

## Vedi anche:
- Documentazione ufficiale di Arduino su `substring()`: https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/substring/
- Tutorial di programmazione su `substring()`: https://www.tutorialspoint.com/arduino/arduino_substrings.htm
- Ulteriori esempi e spiegazioni su `substring()`: https://www.circuitbasics.com/arduino-string-substring-function/