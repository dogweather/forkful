---
title:                "Transformer une date en chaîne."
html_title:           "Java: Transformer une date en chaîne."
simple_title:         "Transformer une date en chaîne."
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous utilisez Java pour travailler avec des dates, il peut être utile de savoir comment convertir une date en chaîne de caractères. Cela peut être nécessaire pour afficher la date dans un format spécifique ou pour la stocker dans une base de données.

## Comment faire

Pour convertir une date en chaîne de caractères en utilisant Java, vous pouvez utiliser la classe DateFormat ou SimpleDateFormat. Voici un exemple de code qui utilise la classe SimpleDateFormat pour convertir la date actuelle en une chaîne au format "yyyy-MM-dd".

```Java
// Créer un objet SimpleDateFormat avec le format souhaité
SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd");

// Récupérer la date actuelle
Date now = new Date();

// Utiliser la méthode format() pour convertir la date en chaîne de caractères
String stringDate = sdf.format(now);

// Afficher la chaîne de caractères
System.out.println(stringDate);

// Output : "2021-10-24"
```

Il est important de noter que le format de la chaîne fournie à la classe SimpleDateFormat doit correspondre au format de la date que vous souhaitez convertir. La documentation de la classe peut être utile pour trouver différents symboles à utiliser pour le formatage.

## Plongée en profondeur

En utilisant la classe SimpleDateFormat, vous pouvez également effectuer des conversions inverses, c'est-à-dire convertir une chaîne de caractères en date. Pour cela, vous pouvez utiliser la méthode parse().

```Java
// Créer un objet SimpleDateFormat avec le format souhaité
SimpleDateFormat sdf = new SimpleDateFormat("dd/MM/yyyy");

// Créer une chaîne de caractères représentant une date
String strDate = "24/10/2021";

// Utiliser la méthode parse() pour convertir la chaîne en date
Date date = sdf.parse(strDate);

// Afficher la date
System.out.println(date);

// Output : Sun Oct 24 00:00:00 CEST 2021
```

En plus de la classe SimpleDateFormat, Java propose également d'autres classes pour gérer les dates et les heures, telles que la classe Calendar et la classe LocalDateTime. Il peut être utile d'explorer ces autres options pour trouver celle qui convient le mieux à votre application.

## Voir aussi

- [Documentation de la classe DateFormat](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/text/DateFormat.html)
- [Documentation de la classe SimpleDateFormat](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/text/SimpleDateFormat.html)
- [Guide de formatage des dates en Java](https://www.baeldung.com/java-simpledateformat)