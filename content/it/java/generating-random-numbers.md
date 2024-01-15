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

## Perché

Se sei un programmatore Java, è molto probabile che ti troverai a dover generare numeri casuali per diversi motivi. Potresti aver bisogno di simularli per scopi di testing, di creare un gioco o di estrarre numeri casuali per un'altra funzionalità del tuo programma.

## Come Generare Numeri Casuali in Java

Per generare numeri casuali in Java, possiamo utilizzare la classe `Random` che fa parte della libreria standard `java.util`. Possiamo inizializzarla con un seed specifico o senza, a seconda delle nostre esigenze. Di seguito un esempio di codice che genera un numero intero casuale compreso tra 1 e 10 inclusi:

```Java
import java.util.Random;

public class RandomNumberGenerator {
    public static void main(String[] args) {
        Random random = new Random(); // inizializza l'oggetto Random

        int randomNumber = random.nextInt(10) + 1; // genera un numero casuale tra 1 e 10
        System.out.println("Numero casuale: " + randomNumber);
    }
}
```

L'output potrebbe essere:

```
Numero casuale: 8
```

Possiamo anche generare un numero casuale tra un range di numeri specificato, utilizzando il metodo `nextDouble()`. Ad esempio, se vogliamo generare un numero con due decimali compreso tra 0 e 1, possiamo utilizzare il seguente codice:

```Java
double randomDecimal = random.nextDouble(); // genera un numero decimale tra 0 e 1
System.out.println("Numero decimale: " + String.format("%.2f", randomDecimal)); // formatta il numero con due decimali
```

L'output potrebbe essere:

```
Numero decimale: 0.63
```

Per generare numeri casuale tra range diversi, possiamo utilizzare il metodo `nextInt(int bound)` dove `bound` rappresenta il limite superiore del range.

## Approfondimento sulla Generazione di Numeri Casuali

Il metodo `nextDouble()` utilizza l'algoritmo di generazione di numeri casuali chiamato "Mersenne Twister". Questo algoritmo è considerato molto efficiente e produce numeri con una buona distribuzione uniforme. Tuttavia, è importante tenere presente che i numeri generati non sono veramente casuali, ma sono determinati dall'algoritmo e dal seed utilizzato per inizializzare l'oggetto `Random`.

Inoltre, se vogliamo generare numeri casuale per scopi critici come il gioco d'azzardo o la crittografia, è consigliato utilizzare la classe `SecureRandom` invece di `Random`. Questa classe utilizza un algoritmo basato sugli input della casuale del sistema operativo e dei dispositivi hardware, rendendo i numeri generati più imprevedibili e sicuri.

## Vedi Anche

- [Documentazione ufficiale di Oracle su Random in Java](https://docs.oracle.com/javase/8/docs/api/java/util/Random.html)
- [Articolo su SecureRandom e la generazione di numeri casuale sicuri in Java] (https://www.baeldung.com/java-securerandom)