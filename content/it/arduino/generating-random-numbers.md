---
title:    "Arduino: Generazione di numeri casuali"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/arduino/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Perché

Generare numeri casuali è un'attività fondamentale nella programmazione di Arduino. È utile per creare giochi, simulazioni o per aggiungere una componente di casualità ai tuoi progetti. In questo articolo, imparerai come generare numeri casuali utilizzando Arduino.

## Come fare

Per generare numeri casuali in Arduino, puoi utilizzare la funzione `random()` che restituisce un valore compreso tra 0 e il numero massimo che gli viene passato come parametro. Ad esempio, per generare un numero casuale tra 1 e 10, puoi utilizzare il seguente codice:

```Arduino
int randomNumber = random(1, 11);
```

Puoi anche utilizzare la funzione `randomSeed()` per impostare un valore di partenza per la generazione dei numeri casuali. Questo è utile se vuoi ottenere sempre gli stessi risultati ogni volta che esegui il tuo programma. Per esempio:

```Arduino
randomSeed(1234);
int randomNumber = random(1, 11); // restituirà sempre lo stesso numero
```

Potresti anche voler generare numeri casuali con decimali. In questo caso, puoi utilizzare la funzione `random()` e poi dividerla per un numero per ottenere un numero con la precisione desiderata. Ad esempio, se vuoi generare un numero casuale tra 1 e 5 con una cifra decimale, puoi fare così:

```Arduino
float randomNumber = random(1, 6) / 10.0; // restituirà un numero con una cifra decimale tra 1.0 e 1.5
```

## Approfondimento

La generazione di numeri casuali non è una cosa del tutto casuale, ma è determinata da un algoritmo che produce una sequenza prevedibile di numeri. In Arduino, viene utilizzato l'algoritmo Park-Miller-Carta per generare numeri casuali.

Se hai bisogno di una sequenza più complessa e meno prevedibile di numeri casuali, puoi utilizzare la libreria `random()` di Arduino che contiene algoritmi come la generazione di numeri casuali a 8, 16 o 32 bit e i numeri casuali basati sull'orologio del sistema.

## Vedi anche

- [Documentazione ufficiale di Arduino per la funzione `random()`](https://www.arduino.cc/reference/en/language/functions/random-numbers/)
- [Spiegazione dettagliata dell'algoritmo Park-Miller-Carta](https://www.firstpr.com.au/dsp/rand31/p1192-park.pdf)
- [Libreria `random()` di Arduino](https://www.arduino.cc/en/Reference/Random)