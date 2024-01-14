---
title:    "Arduino: Generazione di numeri casuali"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Perché
Generare numeri casuali è un passo importante per molti progetti di Arduino. Ciò può essere utile per creare giochi, simulazioni o per dare un tocco di casualità a un qualsiasi programma.

## Come
Per generare numeri casuali in Arduino, possiamo utilizzare la funzione ```random()``` che ci permette di ottenere un numero casuale compreso tra 0 e 65535. Possiamo poi utilizzare alcune operazioni matematiche per ottenere un numero casuale nel range desiderato.

Per esempio, possiamo utilizzare la funzione ```map()``` per mappare il numero casuale nel range che ci interessa. Vediamo un esempio di codice:

```Arduino
int randomNumber = random(65535); // generiamo un numero casuale da 0 a 65535
int mappedNumber = map(randomNumber, 0, 65535, 0, 100); // mappiamo il numero casuale in un range da 0 a 100
Serial.println(mappedNumber); // visualizziamo il numero casuale mappato nella console seriale
```

In questo modo, otterremo un numero casuale compreso tra 0 e 100.

## Approfondiamo
Esistono diverse tecniche per generare numeri casuali in Arduino, ognuna con i suoi vantaggi e svantaggi. Una delle tecniche più comuni è quella di utilizzare il valore analogico di una porta come seme per la funzione ```random()```.

Un altro approccio è quello di utilizzare una libreria esterna specifica per la generazione di numeri casuali, come ad esempio la libreria "RandomLib".

È importante ricordare che, nonostante il termine "casuale", i numeri generati da Arduino saranno sempre deterministici, ovvero seguono un determinato algoritmo. Ciò significa che se eseguiamo più volte lo stesso codice, otterremo sempre gli stessi numeri casuali.

## Vedi anche
- [Documentazione ufficiale di Arduino sulla funzione random()](https://www.arduino.cc/reference/en/language/functions/random-numbers/random/)
- [Tutorial su come generare numeri casuali in Arduino](https://www.arduino.cc/en/Tutorial/RandomNumbers)
- [Libreria RandomLib per la generazione di numeri casuali in Arduino](https://www.arduino.cc/reference/en/libraries/randomlib/)