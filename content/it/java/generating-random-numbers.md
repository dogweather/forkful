---
title:                "Java: Generazione di numeri casuali"
simple_title:         "Generazione di numeri casuali"
programming_language: "Java"
category:             "Java"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Perché

Generare numeri casuali è un'attività comune e utile nella programmazione Java. Può essere utilizzata per creare giochi, testare algoritmi o per qualsiasi altra applicazione che richieda un elemento casuale.

## Come

Ci sono diverse classi e metodi in Java che possono essere utilizzati per generare numeri casuali. L'approccio più semplice è utilizzare la classe Random e il suo metodo nextInt(), che restituisce un intero casuale. Esempio:

```Java
import java.util.Random;

public class GeneratoreCasuale {

  public static void main(String[] args) {
    // Inizializza l'istanza della classe Random
    Random rand = new Random();
    // Genera un numero casuale tra 0 e 9 
    int numeroCasuale = rand.nextInt(10);
    System.out.println("Il numero casuale è: " + numeroCasuale);
  }
}
```
Output:
```
Il numero casuale è: 7
```

È possibile specificare un range diverso di numeri utilizzando i parametri del metodo nextInt(). Ad esempio, se si vuole generare un numero casuale compreso tra 1 e 1000, il codice sarebbe il seguente:

```Java
int numeroCasuale = rand.nextInt(1000) + 1;
```

Esistono anche altre classi, come la classe SecureRandom, che utilizza un algoritmo crittografico per garantire numeri ancora più casuali. È anche possibile generare numeri casuali di altri tipi di dati, come numeri decimali o booleani, utilizzando i metodi appositi.

## Approfondimento

È importante comprendere che i numeri generati casualmente non sono realmente casuali, ma sono basati su un algoritmo matematico. Questo significa che se il codice viene eseguito più volte, verranno prodotti gli stessi numeri. Per evitare questo, è possibile impostare un seed per la classe Random, in modo che i numeri generati siano sempre diversi. Esempio:

```Java
Random rand = new Random(12345); // Imposta il seed su 12345
```

Inoltre, i computer non sono in grado di generare veri numeri casuali, poiché tutte le loro azioni sono basate su algoritmi e input specifici. Per questo motivo, è meglio utilizzare i numeri generati casualmente per scopi non critici, come la creazione di giochi o l'implementazione di test.

## Vedi anche

- [Java Random class documentation](https://docs.oracle.com/javase/8/docs/api/java/util/Random.html)
- [Java SecureRandom class documentation](https://docs.oracle.com/javase/8/docs/api/java/security/SecureRandom.html)
- [Java Math class documentation](https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html) (per generare numeri casuali di altri tipi di dati)
- [Articolo sulla generazione di numeri casuali in Java](https://www.baeldung.com/java-random)