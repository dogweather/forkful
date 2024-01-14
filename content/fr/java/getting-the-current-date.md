---
title:    "Java: Obtenir la date actuelle"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Pourquoi

Il est souvent nécessaire de connaître la date actuelle dans un programme Java. Cela peut être utile pour afficher la date dans un format spécifique, effectuer certaines opérations en fonction de la date ou encore pour gérer les tâches planifiées. Dans cet article, nous allons voir comment obtenir la date actuelle en Java et les différentes manières de la manipuler.

## Comment faire

Il existe plusieurs façons d'obtenir la date actuelle en Java. La méthode la plus simple et la plus courante est d'utiliser la classe ```java.util.Date``` et la classe ```java.text.SimpleDateFormat```, qui permet de formater la date selon vos besoins.

```Java
// Importer les classes nécessaires
import java.util.Date;
import java.text.SimpleDateFormat;

// Créer un objet de type Date
Date date = new Date();

// Définir le format de la date souhaité
SimpleDateFormat dateFormat = new SimpleDateFormat("dd/MM/yyyy");

// Afficher la date au format choisi
System.out.println("La date actuelle est : " + dateFormat.format(date));
```

L'exemple ci-dessus affichera la date actuelle dans le format "jour/mois/année". Vous pouvez également ajuster le format pour inclure l'heure, les minutes et même les millisecondes. La classe ```SimpleDateFormat``` propose différentes options pour formater la date selon vos préférences.

Il est également possible d'obtenir la date actuelle en utilisant la classe ```java.time.LocalDate``` introduite dans Java 8. Cette classe offre plus de fonctionnalités pour manipuler les dates et les heures.

```Java
// Importer la classe nécessaire
import java.time.LocalDate;

// Obtenir la date actuelle
LocalDate date = LocalDate.now();

// Afficher la date
System.out.println("La date actuelle est : " + date);
```

## Plongée en profondeur

Si vous souhaitez en savoir plus sur les différentes façons d'obtenir et de manipuler la date en Java, il est recommandé de se familiariser avec les classes de la bibliothèque date et heure de Java. En plus des classes mentionnées précédemment, il y a d'autres classes telles que ```java.util.Calendar``` et ```java.time.LocalDateTime``` qui peuvent être utiles en fonction de vos besoins.

Il est également important de noter que les dates en Java sont représentées en millisecondes par rapport à une époque fixe. La classe ```java.util.Date``` est basée sur l'époque du 1er janvier 1970, tandis que la classe ```java.time.LocalDate``` utilise une époque différente, le 1er janvier 0001. Cela peut entraîner des différences lors de la manipulation des dates antérieures à 1970.

## Voir aussi

- [Tutoriel Java : manipulation des dates](https://openclassrooms.com/en/courses/26832-apprenez-a-programmer-en-java/5019755-manipulez-les-dates-avec-java)
- [Documentation officielle de Java : classe Date](https://docs.oracle.com/javase/7/docs/api/java/util/Date.html)
- [Documentation officielle de Java : classe LocalDateTime](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDateTime.html)
- [Article sur la gestion des dates avant 1970 en Java](https://fraktalio.com/post/whats-wrong-with-java-date)