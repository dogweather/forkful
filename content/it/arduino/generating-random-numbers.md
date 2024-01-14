---
title:                "Arduino: (Generare numeri casuali)"
simple_title:         "(Generare numeri casuali)"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Perché

Generare numeri casuali è un'attività utilizzata in molti progetti di Arduino per aggiungere un elemento di casualità e rendere i loro dispositivi più divertenti e interessanti. Con l'utilizzo del comando "random()", è possibile creare numeri casuali che possono essere utilizzati in una varietà di modi per migliorare il proprio progetto.

## Come

Per generare numeri casuali con Arduino, è necessario utilizzare la funzione "random()", che restituisce un numero casuale compreso tra 0 e il numero massimo specificato meno 1. Ecco un esempio di come utilizzare questa funzione per generare un numero casuale compreso tra 1 e 10:

```
Arduino random(1, 11);  // numero massimo specificato meno 1

Serial.println(random); // stampa il numero casuale generato sulla console
```

Esempio di output:

```
7
```

In questo esempio, il valore massimo è stato impostato a 11 perché il comando "random()" restituisce un numero compreso tra 0 e il valore massimo meno 1. In questo modo, possiamo ottenere un numero casuale compreso tra 1 e 10.

## Deep Dive

I numeri casuali generati con Arduino sono in realtà numeri pseudocasuali, ossia sono generati utilizzando un algoritmo matematico predefinito che produce una sequenza di numeri apparentemente casuale. Questi numeri possono essere influenzati da fattori esterni, come il rumore elettromagnetico presente nell'ambiente, ma non sono veramente casuali.

Per ottenere numeri più casuali, è possibile utilizzare un sensore esterno, come un sensore di luce o di temperatura, come seme per l'algoritmo di generazione dei numeri casuali.

## See Also

Ecco alcuni link utili per approfondire l'utilizzo di numeri casuali in progetti di Arduino:

- [Tutorial su come utilizzare il comando "random()" di Arduino](https://www.arduino.cc/reference/en/language/functions/random-numbers/random/)
- [Utilizzo di un sensore di luce come seme per generare numeri casuali](https://www.instructables.com/random-number-generator-using-a-photoresistor-and/)
- [Utilizzo di un sensore di temperatura come seme per generare numeri casuali](https://www.instructables.com/Temperature-Rand-o-Lux/)