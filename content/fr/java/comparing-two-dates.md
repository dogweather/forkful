---
title:    "Java: Comparer deux dates"
keywords: ["Java"]
---

{{< edit_this_page >}}

# Pourquoi 

La comparaison de deux dates est un élément essentiel de la programmation en Java. Cela permet de vérifier si une date est plus récente ou plus ancienne que l'autre, ou même si les deux dates sont égales. Cela peut être utile pour diverses applications, telles que la planification d'événements, la gestion de tâches ou la vérification de validité de données.

# Comment faire 

Pour comparer deux dates en Java, vous pouvez utiliser la classe `LocalDate` de la bibliothèque `java.time`. Il existe plusieurs méthodes pour comparer des dates dans cette classe, mais les plus couramment utilisées sont `isBefore()`, `isAfter()` et `equals()`. Voici un exemple de code qui compare deux dates et imprime le résultat :

```
import java.time.LocalDate;

public class ExempleComparaisonDates {

    public static void main(String[] args) {

        // Définir deux dates
        LocalDate date1 = LocalDate.of(2021, 3, 15);
        LocalDate date2 = LocalDate.of(2020, 5, 20);

        // Comparer si date1 est avant date2
        boolean avant = date1.isBefore(date2);
        
        // Comparer si date1 est après date2
        boolean après = date1.isAfter(date2);
        
        // Comparer si date1 est égale à date2
        boolean égale = date1.equals(date2);

        // Imprimer le résultat
        System.out.println("Date 1 est avant date 2 : " + avant);
        System.out.println("Date 1 est après date 2 : " + après);
        System.out.println("Date 1 est égale à date 2 : " + égale);
    }
    
}

```

Le code ci-dessus produira la sortie suivante :

```
Date 1 est avant date 2 : false
Date 1 est après date 2 : true
Date 1 est égale à date 2 : false
```

# Approfondissement

Il est important de noter que la comparaison de dates utilise l'ordre chronologique, en se basant sur les années, les mois et les jours. Par conséquent, si vous comparez deux dates avec la méthode `equals()`, elles doivent être exactement les mêmes pour retourner `true`. Si vous souhaitez comparer des dates avec plus de flexibilité, vous pouvez utiliser la méthode `isEqual()` qui permet de spécifier une tolérance de comparaison pour les années, les mois ou les jours.

De plus, il est possible de comparer des dates avec des informations de temps en utilisant la classe `LocalDateTime`. Les méthodes de comparaison de dates sont également disponibles dans cette classe, telles que `isBefore()`, `isAfter()` et `isEqual()`. Cela peut être utile si vous avez besoin de comparer des dates précises jusqu'à l'heure ou la minute.

# Voir aussi

- Documentation officielle de Java pour la classe `LocalDate` : https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html
- Tutoriel Java pour la manipulation de dates et heures : https://www.tutorialspoint.com/java8/java8_datetime_api.htm
- Exemples de comparaison de dates en Java : https://www.baeldung.com/java-compare-dates