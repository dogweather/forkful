---
title:                "Utilizzare le espressioni regolari"
html_title:           "Arduino: Utilizzare le espressioni regolari"
simple_title:         "Utilizzare le espressioni regolari"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Che cosa & Perché?
Usare le espressioni regolari in Arduino significa cercare stringhe specifiche all'interno del codice. I programmatori lo fanno per semplificare il processo di ricerca e manipolazione dei dati in modo più efficiente.

## Come fare:
```Arduino
"\\d+" //(numero) cercare un numero
".*" //(stringa) cercare qualsiasi carattere
"[a-z]+" //(parola) cercare una parola
```

## Esplorazione approfondita:
Le espressioni regolari sono un metodo utilizzato fin dagli anni '50 per il riconoscimento di modelli all'interno di un testo. In alternativa, alcuni programmatori preferiscono utilizzare funzioni come `IndexOf()` per trovare stringhe specifiche. L'implementazione di espressioni regolari in Arduino richiede l'utilizzo della libreria `Regex`, che fornisce metodi per la ricerca e la manipolazione dei dati utilizzando espressioni regolari.

## Vedi anche:
Per saperne di più sulla libreria `Regex` e le sue funzionalità, visitare la documentazione online di Arduino: https://www.arduino.cc/reference/en/libraries/regex/

Per una spiegazione più dettagliata sull'utilizzo delle espressioni regolari, leggere il seguente articolo (in inglese): https://medium.com/factory-mind/regex-tutorial-a-simple-cheatsheet-by-examples-649dc1c3f285