---
title:                "Arduino: Scrivere su errore standard"
simple_title:         "Scrivere su errore standard"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Perché
Scopriamo insieme perché è importante imparare a scrivere su standard error quando si programma con Arduino. Scrivere su standard error è una tecnica utile per identificare e risolvere eventuali errori nel codice, aiutando a migliorare la qualità del programma.

# Come fare
Per scrivere su standard error in Arduino, utilizzeremo la funzione "Serial.println()", specificando come parametro "stderr". Questo invierà l'output agli errori del codice alla console di monitoraggio seriale.

```
Arduino Serial.println("Messaggio di errore", stderr);
```

Un esempio pratico potrebbe essere:

```
int a = 5;
int b = 0;
if (b == 0) {
  Serial.println("Errore: divisione per zero", stderr);
}
```

Questo codice scriverà il messaggio di errore nella console di monitoraggio seriale, aiutandoci a identificare il problema e a risolverlo.

# Approfondimento
Scrivere su standard error è solo una delle tecniche che possono essere utilizzate per individuare e risolvere gli errori nel codice di Arduino. Altre opzioni includono l'utilizzo di LED di segnalazione o la creazione di un registro degli errori. È importante comprendere il concetto di debug e utilizzare le tecniche in modo appropriato per migliorare la qualità dei propri programmi.

# Vedi anche
- [Documentazione ufficiale di Arduino su Serial.println()](https://www.arduino.cc/reference/en/language/functions/communication/serial/println/)
- [Esempio di codice per gestire gli errori su Arduino](https://www.instructables.com/id/Error-Handling-in-Arduino/)
- [Articolo su come scrivere codice efficiente su Arduino](https://allaboutee.com/2012/11/01/arduino-building-efficient-code-that-doesnt-suck-memory/)