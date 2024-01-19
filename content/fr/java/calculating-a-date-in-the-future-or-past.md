---
title:                "Calculer une date dans le futur ou le passé"
html_title:           "Java: Calculer une date dans le futur ou le passé"
simple_title:         "Calculer une date dans le futur ou le passé"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Pourquoi et pourquoi ?

Calculer une date dans le futur ou le passé consiste à ajouter ou soustraire une quantité spécifiée de temps à une date donnée. Les programmeurs le font souvent pour des tâches comme la planification d'événements, les rappels d'anniversaire ou les limites de temps.

## Comment faire :

Prenons un exemple simple utilisant `java.time`— l’API moderne de Java pour la manipulation des dates.

```Java
import java.time.LocalDate;
import java.time.Period;

public class AdderDate {
    public static void main(String[] args) {

        // date d'aujourd'hui
        LocalDate today = LocalDate.now();

        // ajouter 1 mois à la date actuelle
        LocalDate nextMonth = today.plus(Period.ofMonths(1));

        System.out.println("Date actuelle : " + today);
        System.out.println("Date du mois prochain : " + nextMonth);
    }
}
```

Lors de l'exécution de ce code, vous obtiendrez une sortie similaire à celle-ci :

```Java
Date actuelle : YYYY-MM-DD
Date du mois prochain : YYYY-MM-(DD+1)
```

Le code ci-dessus crée une date pour "aujourd'hui", puis crée une autre date qui est "un mois dans le futur" à partir de celle-ci.

## Plongée profonde

Historiquement, les programmeurs en Java utilisaient des classes comme `java.util.Date` et `java.util.Calendar`, mais ces classes étaient difficiles à utiliser. L'API `java.time` a été introduite dans Java 8 pour résoudre ces problèmes et est maintenant la norme.

En termes d'alternatives, vous pouvez également utiliser des bibliothèques tierces comme Joda-Time. Cependant, `java.time` est intégré, puissant, et généralement suffisant pour la plupart des besoins.

La clé de la précision dans le calcul de la date est de bien comprendre comment fonctionnent les fonctions `plus` et `minus`. Ces fonctions modifient les dates tout en tenant compte d'ajustements tels que les mois de différentes longueurs et les années bissextiles.

## Voir aussi

La documentation officielle sur l'API java.time est un excellent endroit pour en savoir plus et approfondir : https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html

La spécification JSR-310, qui définit `java.time`, est un autre bon document de référence : https://www.jcp.org/en/jsr/detail?id=310

Enfin, pour une explication plus détaillée des concepts clés du traitement de la date/heure, consultez le guide d'Oracle : https://www.oracle.com/technical-resources/articles/java/jf14-date-time.html