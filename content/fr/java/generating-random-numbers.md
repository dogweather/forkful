---
title:                "Java: La génération de nombres aléatoires"
programming_language: "Java"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Pourquoi 
Générer des nombres aléatoires peut être utile pour de nombreuses raisons, notamment pour la simulation de données, la création de jeux ou encore la sécurité informatique.

## Comment faire
La génération de nombres aléatoires en Java se fait à l'aide de la classe ```Random```. Voici un exemple de code pour générer un nombre aléatoire entre 0 et 100 :

```Java
import java.util.Random;

public class RandomNumberGenerator {
  public static void main(String[] args) {
    Random rand = new Random();
    int randomNumber = rand.nextInt(101);
    System.out.println("Nombre aléatoire : " + randomNumber);
  }
}
```
Sortie : ```Nombre aléatoire : 76```

Il est également possible de spécifier une plage de nombres à partir de laquelle le nombre aléatoire sera généré. Par exemple, pour générer un nombre entre 50 et 100 :

```Java
int randomNumber = rand.nextInt(51) + 50;
```

Vous pouvez également utiliser la classe ```Math``` pour générer un nombre aléatoire avec des décimales :

```Java
double randomDouble = Math.random();
System.out.println("Nombre aléatoire décimal : " + randomDouble);
```

## Plongée en profondeur 
La classe ```Random``` utilise un algorithme appelé "Linear congruential generator" pour générer des nombres aléatoires. Cet algorithme utilise une formule mathématique pour générer une séquence de nombres pseudo-aléatoires. Cela signifie que, bien que les nombres semblent aléatoires, ils suivent en réalité une certaine logique.

Il est également important de noter que les générateurs de nombres aléatoires sont "semi-aléatoires", car ils sont basés sur une graine (seed) initiale. Cela signifie que si vous utilisez la même graine, vous obtiendrez la même séquence de nombres aléatoires. Par conséquent, il est recommandé de spécifier une graine aléatoire pour éviter les prédictions de ces nombres.

## Voir aussi
- [Documentation officielle de la classe Random en Java](https://docs.oracle.com/javase/8/docs/api/java/util/Random.html)
- [Article sur les générateurs de nombres aléatoires en Java](https://www.baeldung.com/java-generating-random-numbers) (en anglais)
- [Article sur la sécurité des nombres aléatoires en Java](https://smallstep.com/blog/secure-random-numbers-in-java/) (en anglais)