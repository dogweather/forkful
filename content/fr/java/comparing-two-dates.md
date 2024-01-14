---
title:                "Java: Comparaison de deux dates"
simple_title:         "Comparaison de deux dates"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Pourquoi
Saviez-vous qu'il est possible de comparer deux dates en Java? Cela peut sembler être une tâche simple, mais cela peut être utile dans de nombreux scénarios, tels que la vérification de la validité d'une entrée utilisateur ou la planification d'événements en fonction de dates spécifiques.

## Comment faire
Pour comparer deux dates en Java, nous pouvons utiliser la classe `LocalDate` du package `java.time`. Tout d'abord, nous devons définir deux objets `LocalDate` avec les dates à comparer:

````Java
LocalDate date1 = LocalDate.of(2020, 6, 15);
LocalDate date2 = LocalDate.of(2020, 8, 25);
````
Ensuite, nous pouvons utiliser la méthode `compareTo()` pour comparer ces deux dates, qui renvoie une valeur entière:

````Java
int result = date1.compareTo(date2);
````

Si `result` est égal à 0, cela signifie que les deux dates sont égales. Si `result` est supérieur à 0, cela signifie que `date1` est après `date2`. Si `result` est inférieur à 0, cela signifie que `date1` est avant `date2`.

````Java
if (result == 0) {
    System.out.println("Les deux dates sont égales.");
} else if (result > 0) {
    System.out.println("Date1 est après date2.");
} else {
    System.out.println("date1 est avant date2.");
}
// Output: Date1 est après date2.
````

## Plongeon en profondeur
Il est important de noter que la méthode `compareTo()` compare uniquement les dates en termes de valeurs calendaires, et non pas en prenant en compte d'autres facteurs tels que les fuseaux horaires ou les heures. Pour une comparaison plus précise, nous pouvons utiliser la méthode `isEqual()` pour vérifier si les dates sont égales, ou `isBefore()` et `isAfter()` pour vérifier si une date est avant ou après l'autre en prenant en compte les heures et les minutes.

## Voir aussi
- [Documentation officielle sur la classe LocalDate](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [Guide de référence rapide de la classe LocalDate](https://www.baeldung.com/java-date-compare)
- [Exemples de comparaison de dates en Java](https://www.geeksforgeeks.org/compare-two-dates-in-java/)