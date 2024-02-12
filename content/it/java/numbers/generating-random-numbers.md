---
title:                "Generazione di numeri casuali"
aliases:
- /it/java/generating-random-numbers.md
date:                  2024-01-27T20:34:12.101014-07:00
model:                 gpt-4-0125-preview
simple_title:         "Generazione di numeri casuali"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Cosa e Perché?

Generare numeri casuali consiste nel produrre sequenze o valori singoli imprevedibili all'interno di un intervallo definito. I programmatori utilizzano questa tecnica per vari motivi, inclusi simulazioni, giochi, applicazioni di sicurezza e metodi di campionamento per testare algoritmi in diverse condizioni.

## Come fare:

In Java, la generazione di numeri casuali può essere realizzata utilizzando la classe `Random` del pacchetto `java.util`, o le classi `ThreadLocalRandom` e `SecureRandom` per casi d'uso specifici. Gli esempi seguenti illustrano come utilizzare queste classi.

### Usando la classe `Random`
La classe `Random` offre un modo per generare numeri pseudo-casuali semplici.

```Java
import java.util.Random;

public class RandomExample {
    public static void main(String[] args) {
        Random rand = new Random(); // Crea un oggetto Random

        int randInt = rand.nextInt(50); // Genera un intero casuale da 0 a 49
        double randDouble = rand.nextDouble(); // Genera un double casuale tra 0.0 e 1.0
        boolean randBoolean = rand.nextBoolean(); // Genera un booleano casuale
        
        System.out.println("Intero Casuale: " + randInt);
        System.out.println("Double Casuale: " + randDouble);
        System.out.println("Booleano Casuale: " + randBoolean);
    }
}
```

### Usando la classe `ThreadLocalRandom`
Per applicazioni concorrenti, `ThreadLocalRandom` è più efficiente di `Random`.

```Java
import java.util.concurrent.ThreadLocalRandom;

public class ThreadLocalRandomExample {
    public static void main(String[] args) {
        int randInt = ThreadLocalRandom.current().nextInt(1, 101); // Da 1 a 100
        double randDouble = ThreadLocalRandom.current().nextDouble(1.0, 10.0); // Da 1.0 a 10.0
        
        System.out.println("Intero Casuale: " + randInt);
        System.out.println("Double Casuale: " + randDouble);
    }
}
```

### Usando la classe `SecureRandom`
Per operazioni crittografiche, `SecureRandom` fornisce un livello di sicurezza superiore.

```Java
import java.security.SecureRandom;

public class SecureRandomExample {
    public static void main(String[] args) {
        SecureRandom secRand = new SecureRandom();
        
        byte[] bytes = new byte[20];
        secRand.nextBytes(bytes); // Riempie bytes con numeri casuali sicuri
        
        System.out.println("Byte Casuali Sicuri:");
        for (byte b : bytes) {
            System.out.printf("%02x ", b);
        }
    }
}
```

## Approfondimento

La generazione di numeri casuali si è evoluta significativamente dai primi giorni del computing. La classe `Random` di Java utilizza una formula congruenziale lineare per generare numeri pseudo-casuali, che sono deterministici e non adatti per applicazioni ad alta sicurezza. Questo ha portato all'introduzione di `SecureRandom`, che utilizza algoritmi più sofisticati (ad esempio, SHA1PRNG) per produrre numeri casuali crittograficamente forti.

Tuttavia, `Random` e `SecureRandom` hanno i loro svantaggi, come il degrado delle prestazioni in ambienti multithreading. La classe `ThreadLocalRandom` è stata introdotta in Java 7 per affrontare questo problema fornendo generatori di numeri casuali locali al thread, migliorando significativamente le prestazioni in applicazioni concorrenti.

Sebbene queste classi coprano la maggior parte delle esigenze, per requisiti estremamente elevati o specializzati, gli sviluppatori potrebbero esplorare librerie aggiuntive o sviluppare soluzioni personalizzate. È essenziale scegliere l'approccio giusto in base alle esigenze di sicurezza e ai requisiti di prestazione del caso d'uso.
