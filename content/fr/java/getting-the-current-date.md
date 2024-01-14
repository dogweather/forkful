---
title:    "Java: Obtenir la date actuelle"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Pourquoi

Avoir la date actuelle est une tâche courante dans de nombreux programmes Java. Cela peut être utile pour afficher la date dans un format personnalisé, pour effectuer des calculs de durée ou pour simplement marquer la date et l'heure à laquelle un certain événement s'est produit. Alors, pourquoi devriez-vous apprendre à obtenir la date actuelle en Java ?

## Comment faire

Il existe plusieurs façons de récupérer la date actuelle en Java, chacune avec ses propres avantages et inconvénients. Voici deux méthodes couramment utilisées :

```java
// Méthode 1: Utiliser la classe java.util.Date
Date date = new Date(); // Crée un objet Date avec la date et l'heure actuelles
System.out.println(date); // Affiche la date dans le format par défaut

// Méthode 2: Utiliser la classe java.time.LocalDate
LocalDate localDate = LocalDate.now(); // Crée un objet LocalDate avec la date actuelle
System.out.println(localDate); // Affiche la date sous la forme "aaaa-mm-jj"
```

La sortie de ces deux exemples sera similaire, mais la méthode 2 utilise la nouvelle API de dates introduite en Java 8 et est recommandée pour une utilisation plus moderne.

Il est également possible de formater la date selon vos besoins en utilisant la classe java.time.format.DateTimeFormatter. Voici un exemple qui affiche la date au format "jour/mois/année" :

```java
DateTimeFormatter formatter = DateTimeFormatter.ofPattern("dd/MM/yyyy");
// Utilisez la méthode format pour obtenir une chaîne de caractères avec la date formatée
String formattedDate = formatter.format(localDate);
System.out.println("Date actuelle formatée: " + formattedDate);
```

## Plongée en profondeur

Si vous souhaitez en savoir plus sur la récupération de la date actuelle en Java, vous pouvez également utiliser la classe java.util.Calendar ou explorer les autres classes disponibles dans la nouvelle API de dates. De plus, vous pouvez également effectuer des calculs avec la date, tels que la différence entre deux dates ou l'ajout d'un certain nombre de jours ou d'heures à la date actuelle.

## Voir aussi

- [Documentation officielle de Oracle sur la classe Date](https://docs.oracle.com/javase/8/docs/api/java/util/Date.html)
- [Documentation officielle de Oracle sur la classe LocalDate](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [Article sur les nouvelles API de dates en Java 8](https://www.baeldung.com/java-8-date-time-intro)
- [Guide de formatage de date en Java](https://www.baeldung.com/java-date-format)