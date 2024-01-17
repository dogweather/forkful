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

# Qu'est-ce que c'est et pourquoi le comparer?

Comparer deux dates en programmation consiste à vérifier si deux dates différentes sont avant, après ou égales l'une à l'autre. Les programmeurs le font souvent pour déterminer la chronologie entre deux événements ou pour calculer la durée entre deux dates.

## Comment faire:

Pour comparer deux dates en Java, vous pouvez utiliser la méthode ```compareTo()```. Cette méthode renvoie un entier négatif si la date appelante est antérieure à la date en paramètre, un entier positif si elle est postérieure, et zéro si les deux dates sont égales.

Exemple de code:

```
LocalDate date1 = LocalDate.of(2020, 1, 1);
LocalDate date2 = LocalDate.of(2021, 1, 1);

int result = date1.compareTo(date2);

System.out.println(result); // Output: -1
```

Dans cet exemple, la date1 est antérieure à la date2, donc la méthode ```compareTo()``` renvoie -1.

## Plongée en profondeur:

Historiquement, la comparaison de dates a été un défi pour les programmeurs car elle nécessitait souvent une manipulation complexe de valeurs numériques. Mais avec l'introduction du package ```java.time``` en Java 8, la comparaison de dates est devenue plus simple et plus précise.

Alternativement, vous pouvez également utiliser la méthode ```isBefore()``` pour vérifier si une date est avant une autre, ou ```isAfter()``` pour vérifier si une date est après une autre. Ces méthodes renvoient un booléen (true ou false) au lieu d'un entier comme ```compareTo()```.

Il est important de noter que la comparaison de dates en Java se fait en comparant leur ordre chronologique et non leurs valeurs absolues. Par exemple, le 31 décembre 2020 est après le 1er janvier 2020, même si 31 est une valeur numérique inférieure à 1.

## Voir aussi:

Pour en savoir plus sur les méthodes de comparaison de dates en Java, vous pouvez consulter la documentation officielle de Java à ce sujet. Vous pouvez également trouver des exemples utiles et des astuces en ligne pour résoudre des problèmes courants liés à la comparaison de dates en programmation.