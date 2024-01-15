---
title:                "Comparaison de deux dates"
html_title:           "Java: Comparaison de deux dates"
simple_title:         "Comparaison de deux dates"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Pourquoi

Si vous êtes un développeur Java, vous savez sûrement que les dates sont un élément courant dans la plupart des applications que vous créez. Parfois, vous pourriez avoir besoin de comparer deux dates pour vérifier si elles sont identiques ou si l'une est antérieure ou postérieure à l'autre. Dans cet article, nous allons vous montrer comment comparer deux dates en Java et pourquoi cela peut être utile.

# Comment faire

La comparaison de deux dates se fait en utilisant la classe `LocalDate` de Java. Voici un exemple de code montrant comment créer deux dates et les comparer :

```Java
LocalDate date1 = LocalDate.of(2021, 10, 15); // Crée une date du 15 octobre 2021
LocalDate date2 = LocalDate.now(); // Crée une date correspondant à la date actuelle
int comparison = date1.compareTo(date2); // Compare les deux dates en retournant un entier
```

La méthode `compareTo` retourne un entier négatif si la première date est antérieure à la deuxième, un entier positif si elle est postérieure et 0 si les deux dates sont identiques. Vous pouvez également utiliser les méthodes `isBefore` et `isAfter` pour obtenir des résultats de type booléen.

# Plongée en profondeur

La classe `LocalDate` utilise le système de temps ISO pour représenter les dates. Cela signifie que les dates sont représentées en utilisant l'année, le mois et le jour, dans cet ordre. Il est important de noter que les années bissextiles sont prises en compte dans les calculs.

Si vous avez besoin de comparer des dates avec une précision plus fine comme les heures, les minutes ou les secondes, vous pouvez utiliser la classe `LocalDateTime` qui inclut ces informations.

De plus, la classe `LocalDate` offre également d'autres méthodes utiles telles que `plusDays`, `minusMonths` et `withYear` pour effectuer des opérations sur les dates.

# Voir aussi

- [Documentation officielle de la classe LocalDate](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [Tutoriel vidéo sur la manipulation des dates en Java](https://www.youtube.com/watch?v=BXkiZoQ4VU4)
- [Exemples de comparaisons de dates en Java](https://www.baeldung.com/java-compare-dates)