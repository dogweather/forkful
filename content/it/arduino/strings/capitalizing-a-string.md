---
title:                "Capitalizzare una stringa"
aliases: - /it/arduino/capitalizing-a-string.md
date:                  2024-02-03T19:04:56.378902-07:00
model:                 gpt-4-0125-preview
simple_title:         "Capitalizzare una stringa"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cos'è & Perché?
Capitalizzare una stringa implica convertire il primo carattere di ogni parola in una stringa in maiuscolo, assicurando che il resto rimanga in minuscolo. Questa operazione è comune nella formattazione dei dati e nella normalizzazione dell'input dell'utente per mantenere la coerenza e migliorare la leggibilità.

## Come fare:
Arduino, principalmente noto per l'interazione con l'hardware, include anche capacità di base di manipolazione delle stringhe attraverso il suo oggetto `String`. Tuttavia, manca di una funzione `capitalize` diretta vista nei linguaggi di livello superiore. Quindi, implementiamo la capitalizzazione iterando su una stringa e applicando trasformazioni di maiuscolo/minuscolo.

Ecco un esempio base senza utilizzare librerie di terze parti:

```cpp
String capitalizeString(String input) {
  if (input.length() == 0) {
    return ""; // Restituisce una stringa vuota se l'input è vuoto
  }
  input.toLowerCase(); // Converti l'intera stringa in minuscolo all'inizio
  input.setCharAt(0, input.charAt(0) - 32); // Capitalizza il primo carattere
  
  // Capitalizza le lettere che seguono uno spazio
  for (int i = 1; i < input.length(); i++) {
    if (input.charAt(i - 1) == ' ') {
      input.setCharAt(i, input.charAt(i) - 32);
    }
  }
  return input;
}

void setup() {
  Serial.begin(9600);
  String testStr = "hello arduino world";
  String capitalizedStr = capitalizeString(testStr);
  Serial.println(capitalizedStr); // Output: "Hello Arduino World"
}

void loop() {
  // Ciclo vuoto
}
```

Questo frammento di codice definisce una funzione `capitalizeString` che prima converte l'intera stringa in minuscolo per standardizzare il suo caso. Poi capitalizza il primo carattere e qualsiasi carattere che segue uno spazio, capitalizzando efficacemente ogni parola nella stringa di input. Notare che questa implementazione rudimentale assume la codifica dei caratteri ASCII e potrebbe necessitare di aggiustamenti per un pieno supporto Unicode.

Attualmente, non ci sono librerie di terze parti ampiamente adottate specificamente per la manipolazione delle stringhe nell'ecosistema Arduino, principalmente a causa del suo focus sull'interazione hardware e sull'efficienza. Tuttavia, l'esempio fornito è un modo semplice per ottenere la capitalizzazione delle stringhe all'interno dell'ambiente di programmazione di Arduino.
