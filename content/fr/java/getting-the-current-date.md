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

## Quoi & Pourquoi ?
Obtenir la date actuelle est une fonctionnalité courante dans la programmation Java. Cela permet aux programmeurs de connaître la date et l'heure actuelles et de les utiliser dans leurs programmes pour effectuer différentes actions. Par exemple, cela peut être utilisé pour enregistrer un horodatage dans une base de données ou pour créer des fichiers avec un nom de date spécifique.

## Comment faire :
Voici un exemple de code Java simple pour obtenir la date actuelle :

```Java
import java.time.LocalDate;

public class CurrentDateExample {
    public static void main(String[] args) {
        // Utilise la classe LocalDate pour obtenir la date actuelle
        LocalDate current = LocalDate.now();
        // Affiche la date au format jour/mois/année
        System.out.println("La date actuelle est : " + current);
    }
}
```

Voici la sortie de ce code :

```
La date actuelle est : 12/05/2021
```

## Profondeur de plongée :
Obtenir la date actuelle peut sembler simple, mais cela a nécessité des améliorations dans le langage Java au fil des ans. Avant Java 8, la classe Date était utilisée, mais elle n'était pas thread-safe, ce qui signifie qu'elle pouvait causer des problèmes lors d'une utilisation simultanée par plusieurs threads. Avec l'introduction du package java.time en Java 8, les classes LocalDate, LocalDateTime et Instant ont été introduites pour gérer les dates et les heures de manière plus efficace. Il existe également d'autres options telles que l'utilisation d'une bibliothèque externe comme JodaTime pour gérer les dates et les heures.

## Voir aussi :
Pour en savoir plus sur l'utilisation des dates et des heures en Java, vous pouvez consulter les ressources suivantes :

- [Documentation officielle Java : java.time Package](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [Tutoriel Java : Comment travailler avec les dates en Java](https://www.baeldung.com/java-datetime)
- [Site officiel JodaTime](https://www.joda.org/joda-time/)