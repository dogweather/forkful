---
title:                "Java: Génération de nombres aléatoires"
simple_title:         "Génération de nombres aléatoires"
programming_language: "Java"
category:             "Java"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Pourquoi

Générer des nombres aléatoires est une fonctionnalité importante en programmation, car elle permet de créer un élément de hasard dans un programme. Cela peut être utile pour des jeux, des simulations ou des tests de performance. 

## Comment faire

Pour générer des nombres aléatoires en Java, il faut utiliser la classe ```Random```. Voici un exemple de code qui génère un nombre aléatoire entre 1 et 100 et l'affiche à l'écran:

```java
import java.util.Random;

public class GenerateRandomNumber {
  public static void main(String[] args) {
    Random rand = new Random();
    int randomNumber = rand.nextInt(100)+1; // génére des nombres entre 0 et 99, on ajoute 1 pour obtenir un nombre entre 1 et 100
    System.out.println(randomNumber);
  }
}
```

Output:
```
57
```

On peut également spécifier une plage exacte de nombres en utilisant la méthode ```next```, par exemple ```rand.next(50,150);``` génère un nombre aléatoire entre 50 et 150. 

## Plongée en profondeur

La classe ```Random``` utilise un générateur de nombres pseudo-aléatoire, qui utilise une formule mathématique pour produire une séquence de nombres qui semble aléatoire. Cependant, ces nombres peuvent être prévisibles si on connaît la formule utilisée. 

Pour éviter cela, il est possible d'utiliser une graine (seed en anglais) qui est un nombre de départ fixe pour le générateur aléatoire. Si on utilise la même graine à chaque fois, la séquence de nombres aléatoires sera toujours la même. Pour générer une graine aléatoire, on peut utiliser ```System.currentTimeMillis()``` qui renvoie le nombre de millisecondes écoulées depuis le 1er Janvier 1970. 

Aussi, en utilisant la méthode ```setSeed()```, on peut fixer une graine spécifique pour que la séquence de nombres aléatoires soit toujours la même. Par exemple:

```java
import java.util.Random;

public class GenerateRandomNumber {
  public static void main(String[] args) {
    Random rand = new Random();
    rand.setSeed(1234);
    int randomNumber1 = rand.nextInt(100)+1;
    int randomNumber2 = rand.nextInt(100)+1;
    int randomNumber3 = rand.nextInt(100)+1;
    System.out.println(randomNumber1); // 60
    System.out.println(randomNumber2); // 92
    System.out.println(randomNumber3); // 50
  }
}
```

## Voir aussi

- [Documentation Java sur la classe Random](https://docs.oracle.com/javase/8/docs/api/java/util/Random.html)
- [Article sur la génération de nombres aléatoires en Java](https://www.baeldung.com/java-random)
- [Exemple de jeu de devinette en Java utilisant la classe Random](https://www.geeksforgeeks.org/guess-number-game-java/)