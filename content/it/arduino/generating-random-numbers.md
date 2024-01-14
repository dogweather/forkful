---
title:                "Arduino: Generazione di numeri casuali"
programming_language: "Arduino"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Perché

Il generatore di numeri casuali è uno strumento utile per creare varietà e casualità nei progetti di Arduino. Può essere utilizzato per creare giochi, animazioni o contenuti multimediali interattivi.

## Come fare

Per generare numeri casuali in Arduino, è possibile utilizzare la funzione ```random(min, max)```, che restituisce un numero intero compreso tra il valore minimo e massimo specificato. Ad esempio, se si vuole generare un numero casuale tra 1 e 10, il codice sarebbe:

```Arduino
int numero = random(1, 10); 
```

È importante anche impostare il seme del generatore di numeri casuali utilizzando la funzione ```randomSeed()```, in modo da ottenere numeri differenti ogni volta che si esegue il programma.

## Approfondimento

La funzione ```random(min, max)``` utilizza un algoritmo chiamato "linear congruential generator" (LCG) per generare i numeri casuali. Questo algoritmo utilizza una formula matematica per produrre una sequenza pseudo-casuale di numeri, che a loro volta sono influenzati dal seme impostato.

È importante notare che questa sequenza di numeri non è veramente casuale, ma piuttosto prevedibile e ripetibile se si utilizza lo stesso seme. Tuttavia, per scopi pratici, la maggior parte degli utilizzi di numeri casuali in progetti di Arduino non richiedono un alto grado di casualità.

Un'altra opzione per generare numeri casuali più "casuali", è utilizzare un componente esterno come il sensore di luce o di temperatura, poiché questi valori possono variare in modo imprevedibile e possono essere utilizzati come seme per il generatore di numeri casuali.

## Vedi anche

- [Funzione random() - Documentazione di Arduino](https://www.arduino.cc/reference/en/language/functions/random-numbers/random/)
- [Come generare numeri casuali in Arduino - Tutorial di programmazione C ++](https://www.arduino.cc/en/Tutorial/Foundations/RandomNumbers)
- [Algoritmo LCG - Wikipedia](https://it.wikipedia.org/wiki/Linear_congruential_generator)