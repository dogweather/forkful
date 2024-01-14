---
title:                "Clojure: Transformer une date en chaîne de caractères"
programming_language: "Clojure"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Il y a plusieurs raisons pour lesquelles vous pourriez avoir besoin de convertir une date en une chaîne de caractères dans votre programme Clojure. Peut-être que vous voulez afficher la date dans un format spécifique, l'utiliser dans une requête API, ou simplement stocker la date sous forme de chaîne dans une base de données. Quelle que soit la raison, cet article vous expliquera comment le faire étape par étape.

## Comment Faire

Tout d'abord, il est important de noter que dans Clojure, les dates sont représentées sous forme de nombres entiers qui représentent le nombre de millisecondes depuis le 1er janvier 1970. C'est ce qu'on appelle l'Epoch Unix. Pour convertir une date en une chaîne de caractères, vous devrez utiliser la fonction `format` de la librairie `java.time`.

```Clojure
(require '[java.time :as time])

(def date (time/local-date 2021 5 12))

(time/format date "dd/MM/yyyy")
;; Output: "12/05/2021"

(time/format date "E, MMM d, yyyy")
;; Output: "Wed, May 12, 2021"
```

Comme vous pouvez le voir dans les exemples ci-dessus, la fonction `format` prend deux arguments: la date à convertir et le format de sortie souhaité. Le "dd" représente le jour, le "MM" le mois et le "yyyy" l'année. Vous pouvez également ajouter des éléments supplémentaires tels que "E" pour le jour de la semaine ou "MMM" pour le mois abrégé. Consultez la documentation de `java.time` pour plus de détails sur les formats de dates possibles.

## Plongée Profonde

La librairie `java.time` offre également la possibilité de formater une date en utilisant des modèles prédéfinis tels que `DateTimeFormatter/ISO_DATE` ou `DateTimeFormatter/ISO_INSTANT`. Ces modèles permettent de convertir la date en une chaîne dans un format standardisé, ce qui peut être très utile lors de la manipulation de données provenant de différentes sources.

```Clojure
(time/format date (DateTimeFormatter/ISO_DATE))
;; Output: "2021-05-12"

(time/format (time/now) (DateTimeFormatter/ISO_INSTANT))
;; Output: "2021-05-12T14:30:21.275640Z"
```

Une autre chose importante à noter est que la fonction `format` retourne une chaîne de caractères et ne modifie pas la date initiale. Si vous souhaitez modifier la date elle-même, vous devrez utiliser la fonction `parse` de `java.time` pour convertir une chaîne en objet `java.time.LocalDate`.

## Voir Aussi

- [Documentation de `java.time` en français](https://docs.oracle.com/javase/fr/8/docs/api/java/time/package-summary.html)
- [Le guide Clojure des dates et heures](https://clojure.org/guides/java_time)
- [Les différents formats de dates dans `java.time`](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)