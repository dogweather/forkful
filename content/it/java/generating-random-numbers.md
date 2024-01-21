---
title:                "Generazione di numeri casuali"
date:                  2024-01-20T17:49:11.578739-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generazione di numeri casuali"
programming_language: "Java"
category:             "Java"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?
Generare numeri casuali è come tirare un dado virtuale. I programmatori lo fanno per giochi, simulazioni o per avere dati di test variabili.

## How to:
Esempio con `Random`:
```java
import java.util.Random;

public class GeneratoreCasuale {
    public static void main(String[] args) {
        Random random = new Random(); // creazione di un'istanza di Random
        int numeroCasuale = random.nextInt(50); // genera un numero tra 0 e 49
        System.out.println(numeroCasuale); // stampa il numero
    }
}
```

Esempio con `Math.random`:
```java
public class GeneratoreCasuale {
    public static void main(String[] args) {
        double numeroCasuale = Math.random() * 50; // genera un numero da 0.0 a 49.999...
        int numeroIntero = (int) numeroCasuale; // converte in intero
        System.out.println(numeroIntero); // stampa il numero
    }
}
```

## Deep Dive
All'inizio, i numeri casuali venivano generati usando la fisica, come lanciare una moneta, ma era lento e poco pratico per il calcolo. Le prime funzioni di generazione di numeri casuali in linguaggi di programmazione hanno cambiato il gioco. In Java, la classe `Random` usa un seme per creare sequenze di numeri casuali prevedibili, utile per debug. La funzione `Math.random()` è più comoda per uso rapido senza oggetti extra.

Alternativa: `SecureRandom`, per quando serve sicurezza. `SecureRandom` produce output meno prevedibili, è più adeguato per la crittografia.

Dettagli implementativi: `Random` di Java sfrutta algoritmi come Linear Congruential Generator (LCG). Ma ricordati, "casuale" qui non è veramente casuale, ma sufficientemente imprevedibile per la maggior parte degli usi.

## See Also
- JavaDocs per `Random`: https://docs.oracle.com/en/java/javase/15/docs/api/java.base/java/util/Random.html
- JavaDocs per `SecureRandom`: https://docs.oracle.com/en/java/javase/15/docs/api/java.base/java/security/SecureRandom.html
- Tutorial Oracle sui numeri casuali: https://docs.oracle.com/javase/tutorial/essential/environment/rand.html