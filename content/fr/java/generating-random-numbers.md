---
title:    "Java: Génération de nombres aléatoires"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/java/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Pourquoi

Dans la programmation, il est souvent nécessaire de générer des nombres aléatoires pour diverses tâches telles que la simulation, la cryptographie ou encore les jeux. Générer des nombres aléatoires est un outil puissant pour ajouter de l'incertitude et de la variété dans un programme. Dans cet article, nous allons explorer différentes façons de générer des nombres aléatoires en Java.

# Comment faire

La méthode la plus courante pour générer des nombres aléatoires en Java est d'utiliser la classe `math` et sa méthode `random()`. Voici un exemple de code montrant comment générer un nombre aléatoire entre 1 et 10 :

```Java
int randomNumber = (int) (Math.random() * 10 + 1);
System.out.println("Le nombre aléatoire est : " + randomNumber);
```
La méthode `random()` retourne un nombre aléatoire compris entre 0 et 1. En multipliant ce nombre par le maximum souhaité et en ajoutant un décalage, on obtient un nombre aléatoire dans la plage désirée. Dans cet exemple, nous avons ajouté un décalage de 1 pour que le nombre final soit compris entre 1 et 10.

Il est également possible de générer une liste de nombres aléatoires grâce à la classe `Random`. Voici un exemple de code montrant comment générer un tableau de 5 nombres aléatoires entre 1 et 100 :

```Java
Random random = new Random();
int[] randomNumbers = new int[5];

for (int i = 0; i < randomNumbers.length; i++) {
    randomNumbers[i] = random.nextInt(100) + 1;
    System.out.println("Le nombre aléatoire est : " + randomNumbers[i]);
}
```

En utilisant la méthode `nextInt()` de la classe `Random`, nous pouvons spécifier une limite maximum pour notre nombre aléatoire. De plus, en ajoutant un décalage de 1, nous obtenons un nombre aléatoire entre 1 et 100.

# Plongée en profondeur

Il est important de noter que la méthode `random()` de la classe `math` ne génère pas réellement des nombres aléatoires, mais plutôt des nombres pseudo-aléatoires en utilisant un algorithme prédéfini. Cela signifie que si vous utilisez la même séquence de nombres pour générer des nombres aléatoires, vous obtiendrez toujours la même série de résultats.

Pour éviter cela, il est possible de fournir une graine (seed) à la méthode `random()` ou à la classe `Random`. La graine est un nombre qui sert de point de départ pour l'algorithme de génération de nombres pseudo-aléatoires. Si vous utilisez une graine différente à chaque exécution, vous obtiendrez des résultats différents.

# Voir aussi

- [Documentation officielle de la classe Math en Java](https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html)
- [Documentation officielle de la classe Random en Java](https://docs.oracle.com/javase/8/docs/api/java/util/Random.html)
- [Article sur la génération de nombres aléatoires en Java](https://www.geeksforgeeks.org/java-generating-random-numbers-in-a-range/)