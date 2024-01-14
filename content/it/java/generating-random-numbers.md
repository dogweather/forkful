---
title:    "Java: Generazione di numeri casuali"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/java/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Perché

La generazione di numeri casuali è un aspetto fondamentale della programmazione Java, in quanto ha molteplici utilizzi pratici. Ad esempio, può essere utilizzato per creare dati di test casuali, generare una sequenza di numeri casuali per un gioco o per l'implementazione di algoritmi di intelligenza artificiale. Inoltre, la generazione di numeri casuali è una parte essenziale della crittografia e della sicurezza informatica.

## Come fare

Per generare numeri casuali in Java, è necessario utilizzare la classe Random della libreria di base. Di seguito è riportato un esempio di codice che genera 5 numeri casuali compresi tra 1 e 10 e li stampa a schermo:

```
import java.util.Random;

public class RandomNumbers {
  public static void main(String[] args) {
    Random random = new Random();
    for (int i = 0; i < 5; i++) {
      int randomNumber = random.nextInt(10) + 1;
      System.out.println(randomNumber);
    }
  }
}
```

Ecco un possibile output:

```
7
5
10
8
3
```

La classe Random offre anche altri metodi utili per la generazione di numeri casuali, come ad esempio `nextInt(int bound)` che genera un numero compreso tra 0 (incluso) e il limite specificato (escluso) e `nextDouble()` che restituisce un numero decimale casuale compreso tra 0.0 (incluso) e 1.0 (escluso).

## Approfondimento

La generazione di numeri casuali in Java si basa sull'utilizzo di un algoritmo che produce una sequenza di numeri che sembrano essere casuali, ma in realtà sono deterministici. Ciò significa che lo stesso algoritmo, con gli stessi parametri di input, genererà sempre la stessa sequenza di numeri. Pertanto, è importante che il programmatore scegliere l'algoritmo e i parametri giusti in base alle proprie esigenze.

Inoltre, esistono anche altre tecniche per la generazione di numeri casuali in Java, come l'utilizzo della classe SecureRandom per generare numeri random sicuri, cioè impossibili da prevedere o influenzare da parte di un utente malintenzionato.

## Vedi anche

* [Classe Random della libreria di base](https://docs.oracle.com/javase/8/docs/api/java/util/Random.html)
* [Classe SecureRandom della libreria di base](https://docs.oracle.com/javase/8/docs/api/java/security/SecureRandom.html)
* [Generazione di numeri casuali in Java: una guida completa](https://www.baeldung.com/java-random)