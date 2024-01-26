---
title:                "Génération de nombres aléatoires"
date:                  2024-01-20T17:49:30.058525-07:00
model:                 gpt-4-1106-preview
simple_title:         "Génération de nombres aléatoires"
programming_language: "Java"
category:             "Java"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Générer des nombres aléatoires, c'est comme lancer un dé virtuel : on ne prédit jamais le résultat. Les programmeurs s'en servent pour tout, des jeux jusqu'à la sécurité informatique.

## Comment faire :
Pour générer un nombre aléatoire simple en Java, suivez cet exemple :

```java
import java.util.Random;

public class RandomGenerator {
    public static void main(String[] args) {
        Random random = new Random();

        // Générer un nombre entier aléatoire entre 0 et 99
        int randomNumber = random.nextInt(100);
        System.out.println("Nombre aléatoire: " + randomNumber);
    }
}
```

Sortie probable (varie à chaque exécution) :
```
Nombre aléatoire: 42
```

## Plongée profonde :
Historiquement, générer des nombres "vraiment" aléatoires est délicat. Les ordinateurs suivent des codes précis, ce qui les rend prévisibles. La classe `java.util.Random` utilise un générateur pseudo-aléatoire (PRNG), qui est suffisant pour la plupart des applications. Si votre besoin est critique en sécurité, utilisez `java.security.SecureRandom` qui est plus imprévisible. 

Le PRNG a un "seed" (graine) initial, une valeur de départ. Si vous connaissez le seed, vous pouvez reproduire la série de nombres. Pour les jeux et simulations, c'est cool. En sécurité, pas tellement. Par contre, `SecureRandom` utilise des données plus changeantes, comme le bruit de périphériques, le mouvement de la souris, etc.

Alternativement, pour les cas où il faut un contrôle plus fin ou des distributions différentes, vous pourriez explorer d'autres bibliothèques comme Apache Commons Math.

## Voir également :
- [Documentation Oracle pour Random](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/util/Random.html)
- [Oracle SecureRandom Documentation](https://docs.oracle.com/javase/8/docs/api/java/security/SecureRandom.html)
- [Apache Commons Math](http://commons.apache.org/proper/commons-math/)
