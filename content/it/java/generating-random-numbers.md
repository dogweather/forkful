---
title:                "Generazione di numeri casuali"
html_title:           "Arduino: Generazione di numeri casuali"
simple_title:         "Generazione di numeri casuali"
programming_language: "Java"
category:             "Java"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Cos’è e Perché?

Generare numeri casuali è uno degli aspetti cruciali nella programmazione. Lo facciamo per creare dati di test, simulare scenari casuali, e a volte per implementare specifiche funzioni come criptografia e algoritmi di selezione casuale.

## Come si fa:

Ecco un semplice esempio di codice Java per generare un numero casuale:

```Java
import java.util.Random;

public class Esercizio {
    public static void main(String[] args) {
        Random rand = new Random();
        int n = rand.nextInt(50);
        System.out.println("Il numero casuale è: " + n);
    }
}
```
Il `nextInt(50)` genera un numero casuale compreso tra 0 e 49.

Ora, proviamo ad eseguire il codice:

```Java
Il numero casuale è: 27
```
Non sorprendiamoci se otteniamo un numero diverso, perché è casuale!

## Approfondimento

La generazione di numeri casuali non è un concetto nuovo, la sua origine risale all'antica Grecia, quando usavano i dadi per il sorteggio. Oggi, ci affidiamo agli algoritmi che generano pseudo-numeri casuali.

Esistono alternative alla classe `Random` di Java, come `ThreadLocalRandom` e `SecureRandom`. `ThreadLocalRandom` è più efficiente in un ambiente multithread, mentre `SecureRandom` è più sicuro, ma un po' più lento.

Ecco un dettaglio importante: tutti questi generano numeri "pseudo-casuali". Ciò significa che i numeri sembrano casuali, ma se conosci l'iniziativa (seed), puoi riprodurre la stessa sequenza di numeri.

## Approfondimenti

Per ulteriori informazioni sulle classi di generazione di numeri casuali di Java, puoi visitare:

1. [Java Random Number Generation Tutorial](http://docs.oracle.com/javase/tutorial/essential/concurrency/threadlocalrandom.html)
2. [Java Secure Random](https://docs.oracle.com/javase/8/docs/api/java/security/SecureRandom.html)
3. [Java Thread Local Random](https://docs.oracle.com/javase/7/docs/api/java/util/concurrent/ThreadLocalRandom.html)