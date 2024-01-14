---
title:                "Arduino: Calculer une date dans le futur ou le passé"
programming_language: "Arduino"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Pourquoi

La possibilité de calculer une date dans le futur ou le passé peut être utile pour créer des projets interactifs basés sur l'heure ou la date actuelle. Cela peut également être utile pour faire des programmations spécifiques qui nécessitent un temps ou une date précise.

## Comment faire

Pour calculer une date dans le futur, il faut d'abord déterminer la date et l'heure actuelle en utilisant la fonction ```now()```. Ensuite, en utilisant les fonctions disponibles pour les dates telles que ```day()```, ```month()```, ```year()```, ```hour()```, ```minute()``` et ```second()```, vous pouvez ajouter ou soustraire une valeur à chacune de ces fonctions pour obtenir la date désirée.

Par exemple, si vous souhaitez avoir la date exactement un mois après la date actuelle, vous pouvez utiliser ces fonctions pour obtenir la date souhaitée :

```
Arduino

int day = day(now());
int month = month(now());
int year = year(now());

// Ajout d'un mois à la date actuelle
month++;

// Vérification si le mois est décembre
if (month == 13) {
    // Si c'est le cas, on réinitialise le mois à janvier et on augmente l'année de 1
    month = 1;
    year++;
}

// Affichage de la nouvelle date
Serial.println("La date dans un mois sera le " + String(day) + "/" + String(month) + "/" + String(year));
```

Et voilà, vous pouvez maintenant calculer la date dans le futur en fonction de votre besoin en utilisant ces fonctions.

## Plongée en profondeur

Si vous souhaitez être plus précis dans le calcul de la date, il existe également des fonctions pour les heures, les minutes et les secondes telles que ```second()``` et ```minute()```. De plus, vous pouvez également utiliser des bibliothèques externes pour obtenir le temps précis à partir d'un serveur de temps en ligne.

Il est également important de prendre en compte les années bissextiles lors du calcul d'une date dans le futur ou le passé. Vous pouvez le faire en ajoutant une condition supplémentaire dans votre code.

Enfin, gardez à l'esprit que les fonctions de date et d'heure de l'Arduino sont basées sur une horloge interne et peuvent donc être inexactes avec le temps. Il est donc recommandé de les recalibrer périodiquement pour obtenir de meilleurs résultats.

## Voir aussi

- [Documentation officielle de l'Arduino pour les fonctions de date et d'heure](https://www.arduino.cc/reference/en/language/functions/time/)
- [Tutoriel vidéo sur le calcul de date dans l'Arduino](https://www.youtube.com/watch?v=BzUZONL6ECo)
- [Bibliothèque de temps pour Arduino](https://github.com/PaulStoffregen/Time)