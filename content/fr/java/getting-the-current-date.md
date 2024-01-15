---
title:                "Obtenir la date actuelle"
html_title:           "Java: Obtenir la date actuelle"
simple_title:         "Obtenir la date actuelle"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Pourquoi

Obtenir la date actuelle peut sembler être une tâche simple et banale, mais cela peut être extrêmement utile dans de nombreuses situations, que ce soit pour des applications de planification, de suivi du temps ou même d'enregistrement de l'heure à laquelle une action a été effectuée. En utilisant Java, il est facile d'obtenir la date actuelle et de l'afficher dans divers formats, répondant ainsi à de nombreux besoins de programmation.

## Comment faire

Pour obtenir la date actuelle en Java, vous pouvez utiliser la classe `LocalDate` de la bibliothèque standard `java.time`. Vous pouvez créer une instance de cette classe en utilisant la méthode `now()` et la stocker dans une variable :

```Java
LocalDate dateActuelle = LocalDate.now();
```

Ensuite, vous pouvez utiliser cette variable pour afficher la date dans le format souhaité en utilisant les méthodes `format()` et `DateTimeFormatter` :

```Java
DateTimeFormatter formatter = DateTimeFormatter.ofPattern("dd/MM/yyyy");
String dateFormatee = dateActuelle.format(formatter);
System.out.println(dateFormatee);
// Output: 22/01/2021
```

Ci-dessus, nous avons utilisé le modèle `"dd/MM/yyyy"` pour afficher la date sous la forme jour/mois/année. Vous pouvez également utiliser d'autres modèles tels que `"yyyy-MM-dd"` pour afficher la date sous forme d'année-mois-jour. Pour plus de détails sur les modèles de formatage de date en Java, vous pouvez consulter la documentation officielle.

## Plongée en profondeur

La classe `LocalDate` utilise le calendrier grégorien pour gérer les dates, ce qui en fait un choix fiable pour obtenir la date actuelle. Cette classe est également thread-safe, ce qui signifie que plusieurs threads peuvent utiliser une même instance sans causer d'erreurs. De plus, la méthode `now()` renvoie automatiquement la date dans le fuseau horaire de la machine sur laquelle le code est exécuté.

Une autre classe utile de la bibliothèque `java.time` est `LocalDateTime`, qui représente la date et l'heure actuelles. En utilisant cette classe, vous pouvez obtenir non seulement la date, mais aussi l'heure actuelle, ce qui peut être utile dans certaines situations.

Par exemple, si vous souhaitez enregistrer l'heure à laquelle une action a été effectuée, vous pouvez utiliser `LocalDateTime` pour obtenir une précision au niveau des secondes :

```Java
LocalDateTime datetimeActuelle = LocalDateTime.now();
System.out.println(datetimeActuelle);
// Output: 2021-01-22T15:25:48.282819
```

## Voir aussi

- [Documentation officielle de Java sur les méthodes pour la date et le temps](https://docs.oracle.com/javase/tutorial/datetime/overview/index.html)
- [Guide de formatage de date en Java](https://www.baeldung.com/java-datetimeformatter)
- [Tutoriel Java date et heure sur Baeldung](https://www.baeldung.com/java-dates)