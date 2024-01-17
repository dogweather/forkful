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

## Cosa e perché?

La stampa di output di debug è un modo per visualizzare informazioni durante l'esecuzione del codice. I programmatori lo usano per capire come il loro codice sta funzionando e trovare eventuali errori.

## Come:

```Arduino
// Stampare una variabile
int numero = 10;
Serial.print("Il numero è: ");
Serial.println(numero);
```

```Arduino
// Utilizzo della funzione delay per aggiungere un ritardo di 1 secondo
Serial.println("Inizio del conteggio...");
for(int i = 0; i<=10; i++){
  Serial.println(i);
  delay(1000); // Aggiungiamo un ritardo di 1000 millisecondi (1 secondo)
}
```

## Approfondimento:

La stampa di output di debug è stata utilizzata sin dagli inizi della programmazione, quando non c'erano strumenti sofisticati per il debugging disponibili. Oggi ci sono alternative più avanzate come l'utilizzo di un debugger integrato nel software di sviluppo o di librerie specifiche per il debugging.

## Vedi anche:

- [Documentazione ufficiale di Arduino su Serial.print](https://www.arduino.cc/reference/it/language/functions/communication/serial/print/)
- [Tutorial su come utilizzare la funzione Serial.print](https://www.arduino.cc/en/Tutorial/SerialPrint)