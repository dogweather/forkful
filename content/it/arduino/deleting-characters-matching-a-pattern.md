---
title:    "Arduino: Cancellare caratteri corrispondenti a un modello"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Perché

Ci sono molte ragioni per cui potresti voler eliminare dei caratteri corrispondenti a un determinato pattern in un programma Arduino. Talvolta hai bisogno di pulire i dati che arrivano da una sorgente esterna o semplicemente di manipolare dei dati in entrata prima di elaborarli.

## Come

Per eliminare dei caratteri corrispondenti a un pattern in Arduino, puoi utilizzare la funzione "replace" della libreria String. Ad esempio, se vogliamo eliminare tutti gli spazi vuoti " " da una stringa, possiamo utilizzare il seguente codice:

```Arduino
String testo = "Questo è un esempio.";
testo.replace(" ", "");
Serial.println(testo);
```

L'output di questo codice sarà "Questoèunesempio.".

Ci sono anche altre funzioni utili per eliminare caratteri, come "remove", "trim", "substring" e "toCharArray". È importante prestare attenzione ai tipi di dati che stai manipolando e utilizzare le funzioni appropriate per evitare errori.

## Deep Dive

La funzione "replace" accetta due parametri, il primo è il carattere o il pattern da cercare, mentre il secondo è il carattere o la stringa che lo sostituirà. Puoi anche specificare il numero massimo di occorrenze da sostituire aggiungendo un terzo parametro.

Inoltre, è possibile utilizzare espressioni regolari per eliminare caratteri in base a un particolare pattern. Questo può essere molto utile quando si deve gestire grandi quantità di dati e si vuole automatizzare il processo di eliminazione dei caratteri indesiderati.

## Vedi anche

- [Tutorial sulle espressioni regolari in Arduino](https://www.arduino.cc/reference/en/)

- [Documentazione della libreria String](https://www.arduino.cc/reference/en/language/variables/data-types/string/)

- [Esempi di codice per la manipolazione di stringhe in Arduino](https://www.arduino.cc/en/Tutorial/StringConstructors)

Con questi strumenti e conoscenze, sarai in grado di gestire facilmente la manipolazione dei caratteri in Arduino. Buon hacking!