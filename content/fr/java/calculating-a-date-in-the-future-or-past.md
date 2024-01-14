---
title:    "Java: Calcul de la date dans le futur ou le passé."
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/java/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Pourquoi?
Les dates sont un élément essentiel de la programmation Java, et il est souvent nécessaire de calculer une date dans un avenir ou un passé spécifique. Cela peut être utile dans des cas tels que la planification d'événements ou la création de rappels. Dans cet article, nous allons explorer comment calculer une date dans le futur ou le passé en utilisant Java.

## Comment faire

Pour calculer une date dans le futur ou le passé en Java, nous allons utiliser la classe "Calendar". Commençons par importer cette classe dans notre programme :

```Java
import java.util.Calendar;
```

Ensuite, nous pouvons créer une instance de la classe "Calendar" en utilisant la méthode "getInstance()", qui nous donne un objet représentant la date et l'heure actuelles :

```Java
Calendar cal = Calendar.getInstance();
```

Maintenant, si nous voulons calculer une date dans le futur, nous pouvons utiliser la méthode "add()" de la classe "Calendar". Cette méthode prend deux paramètres, le premier étant le champ de la date que nous voulons modifier (tel que "Calendar.MONTH" pour le mois), et le second étant la valeur de l'ajout ou de la soustraction. Par exemple, si nous voulons ajouter 3 mois à la date actuelle, nous pouvons utiliser le code suivant :

```Java
cal.add(Calendar.MONTH, 3);
```

De même, si nous voulons calculer une date dans le passé, nous pouvons utiliser la méthode "add()" en passant une valeur négative. Par exemple, pour soustraire 2 semaines de la date actuelle, nous pouvons utiliser :

```Java
cal.add(Calendar.WEEK_OF_MONTH, -2);
```

Enfin, pour obtenir la date finale, nous pouvons utiliser la méthode "getTime()" de la classe "Calendar" et la stocker dans une variable de type "Date" :

```Java
Date date = cal.getTime();
```

Voici un exemple complet de calcul d'une date dans le futur de 6 mois à partir de la date actuelle :

```Java
import java.util.Calendar;
import java.util.Date;

public class DateCalculator {

    public static void main(String[] args) {
        Calendar cal = Calendar.getInstance();

        // Ajoute 6 mois à la date actuelle
        cal.add(Calendar.MONTH, 6);

        // Stocke la date finale dans une variable
        Date date = cal.getTime();

        // Affiche la date calculée
        System.out.println(date);
    }
}
```

La sortie de ce code sera la date dans 6 mois à partir de maintenant, au format "{jour de la semaine} {mois} {jour} {heure}:{minute}:{seconde} {fuseau horaire} {année}".

## Deep Dive

Même si la classe "Calendar" est utile pour calculer des dates, elle peut entraîner certaines incohérences et erreurs. Pour éviter cela, il est recommandé d'utiliser la bibliothèque "Java Time API" qui est disponible à partir de Java 8.

Cette bibliothèque contient la classe "LocalDate" qui permet de représenter une date sans le temps, et la classe "LocalDateTime" pour représenter une date avec le temps. Voici un exemple de code utilisant ces classes pour calculer une date dans le futur :

```Java
import java.time.LocalDate;

public class DateCalculator {

    public static void main(String[] args) {
        LocalDate currentDate = LocalDate.now();

        // Ajoute une année à la date actuelle
        LocalDate futureDate = currentDate.plusYears(1);

        // Affiche la date calculée
        System.out.println(futureDate);
    }
}
```

En utilisant ces classes, nous évitons les erreurs liées à la manipulation de dates et obtenons un code plus concis et lisible.

## Voir aussi

- Tutoriel Java Time API : https://mkyong.com/java8/java-8-date-and-time-api/
- Documentation officielle de la classe "Calendar" : https://docs.oracle.com/javase/8/docs/api/java/util/Calendar.html
- Documentation officielle de "Java Time API" : https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html