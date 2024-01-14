---
title:                "Java: Generazione di numeri casuali"
programming_language: "Java"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Perché generare numeri casuali in Java?

Generare numeri casuali è un'importante funzionalità della programmazione Java che può essere utile in molte situazioni. Ad esempio, può essere utilizzata per creare giochi, modelli di dati casuali o per testare il codice in modo casuale. In generale, l'utilizzo di numeri casuali può aggiungere una componente di casualità e dinamicità ai programmi.

## Come generare numeri casuali in Java

La classe Random di Java fornisce dei metodi per generare numeri casuali. Per utilizzarli, è necessario importare la classe Random nel tuo codice. Vediamo un esempio di codice che genera un numero intero casuale compreso tra 1 e 10:

```Java
Random random = new Random();
int numeroCasuale = random.nextInt(10) + 1;
System.out.println("Il numero casuale generato è: " + numeroCasuale);
```

In questo caso, stiamo utilizzando il metodo `nextInt()` per generare un numero intero casuale e poi aggiungiamo 1 per ottenere un numero compreso tra 1 e 10. È importante notare che `nextInt()` restituisce un numero compreso tra 0 e il numero scelto meno 1. Quindi, se volessimo generare un numero compreso tra 1 e 100, dovremmo utilizzare `nextInt(100) + 1`.

Oltre a `nextInt()`, esistono molti altri metodi nella classe Random di Java che possono essere utilizzati per generare numeri casuali, come ad esempio `nextDouble()` per generare un numero decimale casuale o `nextBoolean()` per generare un valore booleano casuale.

Per ulteriori informazioni su come utilizzare la classe Random di Java per generare numeri casuali, puoi consultare la documentazione ufficiale di Java [qui](https://docs.oracle.com/en/java/javase/15/docs/api/java.base/java/util/Random.html).

## Approfondimento: come funzionano i numeri casuali in Java

Per generare numeri casuali, la classe Random di Java utilizza un algoritmo matematico che calcola una successione di numeri in modo pseudo-casuale. Ciò significa che i numeri generati non sono veramente casuali, ma sono piuttosto una sequenza prevedibile che sembra casual. L'algoritmo utilizza un seme come punto di partenza per generare questa successione di numeri, quindi se si utilizza lo stesso seme, si otterrà sempre la stessa sequenza.

Per impostazione predefinita, la classe Random di Java utilizza il tempo di sistema come seme, il che significa che ogni volta che si crea un nuovo oggetto Random, il seme sarà diverso e quindi i numeri casuali generati saranno diversi. Tuttavia, è possibile impostare manualmente il seme utilizzando il costruttore della classe Random, ad esempio `Random random = new Random(1234)`, dove 1234 è il seme scelto.

È importante notare che i numeri casuali generati dalla classe Random di Java non sono criptograficamente sicuri e non devono essere utilizzati per scopi di sicurezza. Per questi scopi, esistono altre classi nella libreria di Java, come SecureRandom, che forniscono numeri casuali criptograficamente sicuri.

## Vedi anche

- [Documentazione ufficiale di Java sulla classe Random](https://docs.oracle.com/en/java/javase/15/docs/api/java.base/java/util/Random.html)
- [Tutorial su come generare numeri casuali in Java](https://www.baeldung.com/java-generating-random-numbers)
- [Esempi pratici di utilizzo dei numeri casuali in Java](https://examples.javacodegeeks.com/core-java/util/random/java-util-random-example/)