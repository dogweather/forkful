---
title:                "Generazione di numeri casuali"
html_title:           "Java: Generazione di numeri casuali"
simple_title:         "Generazione di numeri casuali"
programming_language: "Java"
category:             "Java"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Che cos'è e perché?
Generare numeri casuali è una funzione importante nella programmazione, in cui viene utilizzata per creare valori casuali che possono essere utilizzati in diversi contesti. I programmatori spesso utilizzano numeri casuali per creare giochi, simulazioni e algoritmi complessi.

## Come:
Di seguito sono presentati alcuni esempi di codice Java per generare numeri casuali. Eseguire questi esempi produrrà un output casuale ogni volta che viene eseguito.

```java
// Generare un numero intero casuale compreso tra 0 e 10
int randomNumber = (int) (Math.random() * 10);
System.out.println(randomNumber);

// Generare un numero decimale casuale compreso tra 0 e 1
double randomNumber = Math.random();
System.out.println(randomNumber);

// Generare un numero intero casuale compreso tra un valore minimo e massimo specificato
int min = 5;
int max = 15;
int randomNumber = (int) (Math.random() * (max - min + 1)) + min;
System.out.println(randomNumber);
```

## Approfondimento:
La generazione di numeri casuali è stata introdotta nella programmazione negli anni '40 per simulare eventi casuali. Prima dell'avvento dei computer, i numeri casuali venivano generati utilizzando metodi fisici come lanci di dadi o l'estrazione di biglietti da un cappello. Oggi, i programmatori possono utilizzare diversi metodi per generare numeri casuali, come l'algoritmo di generazione lineare congruenziale o l'algoritmo di generazione di combinazioni.

## Vedi anche:
- [Java Math Class Documentation](https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html)
- [Random Number Generation in Java](https://www.baeldung.com/java-random)
- [The History of Random Numbers](https://www.random.org/randomness/)