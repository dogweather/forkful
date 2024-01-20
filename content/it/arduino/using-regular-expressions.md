---
title:                "Utilizzo delle espressioni regolari"
html_title:           "Bash: Utilizzo delle espressioni regolari"
simple_title:         "Utilizzo delle espressioni regolari"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Cos'è & Perché?

Le espressioni regolari (RegEx) sono strumenti usati per abbinare, cercare o sostituire stringhe di testo. I programmatori le utilizzano per rendere il codice più compatto, veloce ed efficiente.

## Come fare:

Con Arduino, possiamo utilizzare la libreria `<regex.h>` per utilizzare le espressioni regolari. Ecco un esempio basilare:

```Arduino
#include <regex.h>

void setup() {
  Serial.begin(9600);
  regex_t regex;
  int result;
  result = regcomp(&regex, "a", 0);
  Serial.println(result);  // restituirà 0 se la compilazione è andata a buon fine
}

void loop() {
  // Cosa inserire qui dipende dalla tua applicazione
}
```

L'output sarà `0`, che indica che l'espressione regolare è stata compilata correttamente.

## Approfondimento

Storicamente, le espressioni regolari derivano dalla teoria delle espressioni regolari in teoria degli automi. Questa tecnica di ricerca e sostituzione di stringhe è stata implementata per la prima volta nel linguaggio di programmazione Perl, ed è ora disponibile in molte lingue, compreso l'Arduino.

Ci sono alternative alle espressioni regolari, naturalmente. Metodi tradizionali come `strstr()`, `strcmp()`, e `sprintf()` possono essere usati, ma potrebbero richiedere più codice e potrebbero non essere così efficienti.

Le espressioni regolari in Arduino sono implementate attraverso l'uso di una libreria, che offre molte funzioni tra cui `regcomp()`, `regexec()`, `regerror()`, e `regfree()`.

## Vedi anche

Per ulteriori informazioni e esempi, consulta le seguenti risorse:

- [Guida alle espressioni regolari di Arduino](https://www.arduino.cc/en/Reference/HomePage)
- [Tutorial dettagliato sulle espressioni regolari](https://www.regular-expressions.info/tutorial.html)
- [Documentazione sulla libreria `<regex.h>` di Arduino](https://www.nongnu.org/avr-libc/user-manual/group__avr__regexp.html)