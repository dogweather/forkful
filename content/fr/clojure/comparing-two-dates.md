---
title:                "Clojure: Comparaison de deux dates"
programming_language: "Clojure"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Pourquoi

Comparer deux dates est une tâche courante en programmation. Cela peut être utile pour trier des données chronologiquement, vérifier des durées ou déterminer l'avancement d'un projet.

## Comment faire

Pour comparer deux dates en Clojure, nous pouvons utiliser la fonction `compare` qui prend en entrée deux dates et renvoie un nombre négatif si la première date est antérieure à la deuxième, un nombre positif si elle est postérieure et zéro si elles sont égales.

Voici un exemple de code avec la fonction `compare` :

```Clojure 
(let [date1 (java.util.Date. 2020 6 15)
      date2 (java.util.Date. 2020 6 20)]
  (println (compare date1 date2)))
; Output: -1 
```

Dans cet exemple, la première date (15 juin 2020) est antérieure à la deuxième date (20 juin 2020), donc la valeur renvoyée est -1.

Nous pouvons également utiliser la comparaison logique avec les opérateurs `<`, `>`, `<=` et `>=` pour vérifier si une date est plus ancienne ou plus récente qu'une autre. Voici un exemple :

```Clojure
(let [date1 (java.util.Date. 2020 6 15)
      date2 (java.util.Date. 2020 6 20)]
  (println (< date1 date2)))
; Output: true 
```

Dans cet exemple, la première date (15 juin 2020) est strictement antérieure à la deuxième date (20 juin 2020), donc le résultat de la comparaison est `true`.

## Plongée en profondeur

En comparant deux dates, il est important de garder à l'esprit que Java, sur lequel Clojure est basé, stocke les dates au format `timestamp` en millisecondes depuis le 1er janvier 1970. Cela signifie que lorsqu'on compare des dates, on compare en réalité leurs valeurs `timestamp`.

De plus, la fonction `compare` fonctionne également pour les dates avec une précision jusqu'à la deuxième, mais pas pour les millisecondes. Si vous devez comparer des millisecondes, vous pouvez utiliser la fonction Clojure `java-time/compare` qui renvoie une valeur décimale représentant la différence en millisecondes entre les deux dates.

## Voir aussi

- [Documentation officielle de la fonction "compare" en Clojure](https://clojuredocs.org/clojure.core/compare)
- [Comparaisons logiques en Clojure](https://clojuredocs.org/clojure.core/<)
- [Documentation officielle de la fonction "java-time/compare" en Clojure](https://clojuredocs.org/clojure.java-time/compare)