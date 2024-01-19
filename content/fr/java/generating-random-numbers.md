---
title:                "Générer des nombres aléatoires"
html_title:           "Elixir: Générer des nombres aléatoires"
simple_title:         "Générer des nombres aléatoires"
programming_language: "Java"
category:             "Java"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Générer des nombres aléatoires en programmation est le processus de création de nombres qui ne peuvent être raisonnablement prédits. C'est essentiel dans divers scénarios, par exemple pour les jeux, la simulation, le cryptage et les tests de logiciels.

## Comment faire :

Voici comment vous pouvez générer un nombre aléatoire entre 1 et 100 en Java.

```Java
import java.util.Random;
    
public class Main {
    public static void main(String[] args) {
        Random rand = new Random();
        int num = rand.nextInt(100) + 1;
        System.out.println("Nombre aléatoire généré : " + num);
    }
}
```

Exécutez ce programme, et vous verrez un nombre aléatoire entre 1 et 100 imprimé dans la console.

   
## Plongée Profonde :

Historiquement, la génération de nombres aléatoires était une tâche manuelle, mais avec les progrès de l'informatique, des techniques efficaces et automatisées ont été développées.

En Java, nous avons plusieurs façons de générer des nombres aléatoires : `Math.random()`, `ThreadLocalRandom.current().nextInt()` et `java.util.Random()` sont quelques exemples. Chacun a ses avantages et inconvénients. Per exemple, `java.util.Random()` utilise un germe de nombre aléatoire interne pour générer des nombres. Si deux instances de `Random` sont créées avec la même graine, elles produiront la même séquence de nombres.

## Voir Aussi :

Pour en savoir plus sur la génération de nombres aléatoires en Java, consultez ces ressources utiles :

1. Oracle Java Documentation: [Class Random](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/util/Random.html)
2. Oracle Java Tutorial: [How to Generate Random Numbers](https://docs.oracle.com/javase/tutorial/essential/math/random.html)
3. Blog Post: [Pseudo-random Numbers in Java](https://www.baeldung.com/java-random)