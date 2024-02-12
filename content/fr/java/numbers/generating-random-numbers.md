---
title:                "Génération de nombres aléatoires"
aliases: - /fr/java/generating-random-numbers.md
date:                  2024-01-27T20:33:54.928487-07:00
model:                 gpt-4-0125-preview
simple_title:         "Génération de nombres aléatoires"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Quoi et pourquoi ?

La génération de nombres aléatoires consiste à produire des séquences ou des valeurs uniques imprévisibles dans une plage définie. Les programmeurs utilisent cette technique pour diverses raisons, notamment les simulations, les jeux, les applications de sécurité et les méthodes d'échantillonnage pour tester des algorithmes dans différentes conditions.

## Comment faire :

En Java, la génération de nombres aléatoires peut être réalisée à l'aide de la classe `Random` du package `java.util`, ou des classes `ThreadLocalRandom` et `SecureRandom` pour des cas d'utilisation spécifiques. Les exemples suivants illustrent comment utiliser ces classes.

### Utilisation de la classe `Random`
La classe `Random` offre un moyen de générer des nombres pseudo-aléatoires simples.

```Java
import java.util.Random;

public class ExempleAleatoire {
    public static void main(String[] args) {
        Random rand = new Random(); // Crée un objet Random

        int randInt = rand.nextInt(50); // Génère un entier aléatoire de 0 à 49
        double randDouble = rand.nextDouble(); // Génère un double aléatoire entre 0.0 et 1.0
        boolean randBoolean = rand.nextBoolean(); // Génère un boolean aléatoire
        
        System.out.println("Entier Aléatoire : " + randInt);
        System.out.println("Double Aléatoire : " + randDouble);
        System.out.println("Boolean Aléatoire : " + randBoolean);
    }
}
```

### Utilisation de la classe `ThreadLocalRandom`
Pour les applications concurrentes, `ThreadLocalRandom` est plus efficace que `Random`.

```Java
import java.util.concurrent.ThreadLocalRandom;

public class ExempleThreadLocalRandom {
    public static void main(String[] args) {
        int randInt = ThreadLocalRandom.current().nextInt(1, 101); // De 1 à 100
        double randDouble = ThreadLocalRandom.current().nextDouble(1.0, 10.0); // De 1.0 à 10.0
        
        System.out.println("Entier Aléatoire : " + randInt);
        System.out.println("Double Aléatoire : " + randDouble);
    }
}
```

### Utilisation de la classe `SecureRandom`
Pour les opérations cryptographiques, `SecureRandom` offre un niveau de sécurité supérieur.

```Java
import java.security.SecureRandom;

public class ExempleSecureRandom {
    public static void main(String[] args) {
        SecureRandom secRand = new SecureRandom();
        
        byte[] bytes = new byte[20];
        secRand.nextBytes(bytes); // Remplit bytes avec des nombres aléatoires sécurisés
        
        System.out.println("Octets Aléatoires Sécurisés :");
        for (byte b : bytes) {
            System.out.printf("%02x ", b);
        }
    }
}
```

## Plongée profonde

La génération de nombres aléatoires a considérablement évolué depuis les débuts de l'informatique. La classe `Random` de Java utilise une formule linéaire congruentielle pour générer des nombres pseudo-aléatoires, qui sont déterministes et ne conviennent pas pour des applications à haute sécurité. Cela a conduit à l'introduction de `SecureRandom`, qui utilise des algorithmes plus sophistiqués (par exemple, SHA1PRNG) pour produire des nombres aléatoires cryptographiquement robustes.

Cependant, `Random` et `SecureRandom` ont leurs lacunes, telles que la dégradation des performances dans les environnements multithreads. La classe `ThreadLocalRandom` a été introduite dans Java 7 pour résoudre ce problème en fournissant des générateurs de nombres aléatoires locaux aux threads, améliorant ainsi considérablement les performances dans les applications concurrentes.

Bien que ces classes couvrent la plupart des besoins, pour des exigences extrêmement élevées ou spécialisées, les développeurs pourraient explorer des bibliothèques supplémentaires ou développer des solutions personnalisées. Il est essentiel de choisir la bonne approche en fonction des besoins de sécurité et des exigences de performance du cas d'utilisation.
