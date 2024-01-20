---
title:                "Stampa dell'output di debug"
html_title:           "Arduino: Stampa dell'output di debug"
simple_title:         "Stampa dell'output di debug"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/printing-debug-output.md"
---

{{< edit_this_page >}}

## Cos'è & Perché?

La stampa dell'output di debug è una tecnica di programmazione usata per tracciare lo svolgimento del codice. Gli sviluppatori lo fanno per trovare facilmente e correggere gli errori logici nel codice tramite un processo chiamato debugging.

## Come Fare:

Ecco un esempio di base di stampa di debug in Arduino:

```Arduino
void setup() {
  // Inizializza il terminale seriale a 9600 bps:
  Serial.begin(9600);
}

void loop() {
  // Stampa il messaggio "Ciao, Mondo!" nel terminale seriale
  Serial.println("Ciao, Mondo!");
  delay(1000); // Attesa di un secondo
}
```

Questo codice invierà il messaggio "Ciao, Mondo!" alla console seriale ogni secondo.

## Approfondimenti:

- Contesto storico: La stampa di debug è tra le tecniche più antiche per il debugging. Da prima dell'Arduino, è stata utilizzata in molte altre piattaforme di programmazione.
- Alternative: Anche se la stampa di debug è utile, ci sono altre tecniche di debugging più potenti, come il debugging step-by-step con un debugger.
- Dettagli di implementazione: Arduino semplicemente usa la classe Serial per implementare la stampa di debug. Questo implica l'uso di funzioni come `Serial.begin(9600);` per iniziare una nuova connessione seriale a 9600 bit al secondo e `Serial.println("Messaggio");` per stampare un messaggio.

## Vedi Anche:

- [StackOverflow: Tecniche di debugging Arduino](https://stackoverflow.com/questions/20457596/how-to-debug-arduino)
- [Progetto Arduino: Serial](https://www.arduino.cc/reference/en/language/functions/communication/serial/)