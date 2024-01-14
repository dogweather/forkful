---
title:    "Java: Génération de nombres aléatoires"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Pourquoi

Générer des nombres aléatoires est une pratique courante en programmation. Cela permet d'introduire un élément de hasard ou de variabilité dans nos programmes, ce qui peut être utile dans de nombreuses applications telles que les jeux, les simulations ou encore la génération de données de test.

## Comment faire

Pour générer des nombres aléatoires en Java, nous pouvons utiliser la classe Random. Cette classe fait partie de la bibliothèque standard de Java et fournit des méthodes pour générer des nombres entiers, des nombres à virgule flottante et même des valeurs booléennes aléatoires.

Voici un exemple de code pour générer un nombre entier aléatoire entre 1 et 10 :

```Java
Random random = new Random();
int nombreAleatoire = random.nextInt(10) + 1;
System.out.println(nombreAleatoire); // affiche un nombre aléatoire entre 1 et 10
```

Et voici un autre exemple pour générer un nombre à virgule flottante aléatoire entre 0 et 1 :

```Java
Random random = new Random();
double nombreAleatoire = random.nextDouble();
System.out.println(nombreAleatoire); // affiche un nombre à virgule flottante aléatoire entre 0 et 1
```

Dans ces exemples, nous utilisons la méthode nextInt() pour générer un nombre entier ou la méthode nextDouble() pour générer un nombre à virgule flottante. Nous pouvons également utiliser d'autres méthodes telles que nextLong() pour générer des nombres longs aléatoires ou nextBoolean() pour générer des valeurs booléennes aléatoires.

## Approfondissement

Bien que la classe Random soit facile à utiliser pour générer des nombres aléatoires, il est important de comprendre que ces nombres ne sont pas véritablement aléatoires. En réalité, la classe utilise un algorithme pour générer des séquences de nombres qui semblent aléatoires, mais qui sont en réalité déterminés par une "graine" de départ.

Si nous voulons générer des nombres véritablement aléatoires, nous pouvons utiliser la classe SecureRandom qui utilise des sources d'entropie externes pour initialiser sa graine et générer des nombres plus sûrs.

Enfin, il est important de noter que la génération de nombres aléatoires peut être un sujet complexe et qu'il existe de nombreuses ressources en ligne pour en apprendre davantage sur les différents algorithmes et meilleures pratiques en matière de génération de nombres aléatoires en Java.

## Voir aussi

- [Documentation officielle de la classe Random en Java](https://docs.oracle.com/javase/8/docs/api/java/util/Random.html)
- [Article sur la génération de nombres aléatoires en Java sur Baeldung](https://www.baeldung.com/java-generate-random-long-float-integer-double)
- [Article sur les meilleures pratiques en matière de génération de nombres aléatoires en Java sur Java World](https://www.javaworld.com/article/2078296/security/safe-and-secure-apis-in-java-to-generate-random-numbers.html)