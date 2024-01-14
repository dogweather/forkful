---
title:                "Arduino: Calcul d'une date dans le futur ou le passé"
simple_title:         "Calcul d'une date dans le futur ou le passé"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Pourquoi

La programmation est une compétence essentielle en informatique qui peut être utile dans de nombreuses situations. Par exemple, savoir comment calculer une date dans le passé ou dans le futur peut être utile dans la programmation de projets tels que des calendriers, des rappels ou des tâches programmées.

# Comment faire

Pour calculer une date dans le passé ou dans le futur, nous allons utiliser la bibliothèque Time d'Arduino. Cette bibliothèque contient des fonctions qui nous permettent de manipuler facilement les dates et les heures.

Tout d'abord, nous devons initialiser notre heure et date actuelles. Nous pouvons utiliser la fonction `now()` pour obtenir la date et l'heure actuelles et stocker les valeurs dans des variables séparées. Par exemple :

```
Arduino ...

time_t currentTime = now();
int currentYear = year(currentTime);
int currentMonth = month(currentTime);
int currentDay = day(currentTime);
```

Ensuite, nous pouvons utiliser la fonction `makeTime()` pour créer un objet Time en spécifiant les valeurs de l'année, du mois, du jour, de l'heure, des minutes et des secondes que nous voulons pour notre date future ou passée. Par exemple, pour créer une date dans 5 jours à 9h30, nous pouvons écrire :

```
Arduino ...

int futureDay = currentDay + 5;
int futureHour = 9;
int futureMinute = 30;

tmElements_t futureTime;
futureTime.Year = currentYear;
futureTime.Month = currentMonth;
futureTime.Day = futureDay;
futureTime.Hour = futureHour;
futureTime.Minute = futureMinute;
futureTime.Second = 0;

time_t futureDateTime = makeTime(futureTime);
```

Enfin, nous pouvons utiliser la fonction `setTime()` pour régler l'heure et la date de notre Arduino au moment spécifié. Par exemple :

```
Arduino ...

setTime(futureDateTime);
```

Et voilà ! Maintenant, notre Arduino affichera la date et l'heure calculées.

# Plongeons plus en profondeur

Il est également possible de calculer des dates dans le passé ou le futur en ajoutant ou en soustrayant des secondes, des minutes, des heures, des jours, des semaines ou des mois à notre heure et date actuelles. Pour cela, nous pouvons utiliser des fonctions telles que `second()`, `minute()`, `hour()`, `day()`, `month()` et `year()` pour obtenir les valeurs de notre date actuelle, puis utiliser les opérateurs arithmétiques tels que `+` et `-` pour modifier ces valeurs.

De plus, la bibliothèque Time d'Arduino offre également des fonctions pratiques pour comparer des dates, vérifier si une année est bissextile et bien plus encore. Pour en savoir plus sur ces fonctions, vous pouvez consulter la documentation officielle de la bibliothèque.

# Voir aussi

- Documentation de la bibliothèque Time d'Arduino : https://www.arduino.cc/en/Reference/Time
- Tutoriel sur la gestion du temps avec Arduino : https://create.arduino.cc/projecthub/Arduino_Genuino/using-the-time-library-af8a7c

Merci d'avoir lu cet article ! Nous espérons que cela vous a été utile dans vos projets de programmation Arduino. N'hésitez pas à explorer davantage la bibliothèque Time et à expérimenter avec différents calculs de date. Bonne programmation !