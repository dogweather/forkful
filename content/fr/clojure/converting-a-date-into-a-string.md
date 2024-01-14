---
title:    "Clojure: Transformer une date en chaîne de caractères."
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un développeur Clojure, vous avez peut-être été confronté à un scénario où vous avez besoin de convertir une date en chaîne de caractères pour affichage ou pour une autre manipulation. Le processus peut sembler simple, mais il est important de comprendre les différentes méthodes et options disponibles pour effectuer cette conversion.

## Comment Faire

La première étape pour convertir une date en chaîne de caractères est de créer une instance du type `java.util.Date` en utilisant les fonctions `date` ou `java.util.Date`. Ensuite, vous pouvez utiliser la fonction `format` de Clojure pour spécifier le format de sortie de la date.

Voici un exemple qui convertit la date actuelle en format jour/mois/année (dd/MM/yyyy) :

```Clojure
(def date (date))
(format date "dd/MM/yyyy") ;; output -> "27/01/2021"
```

Il est également possible de spécifier un fuseau horaire pour la date en utilisant la fonction `with-timezone` avant la fonction `format`. Voici un exemple pour une date dans le fuseau horaire de Paris :

```Clojure
(def date (java.util.Date.))
(format (with-timezone date (timezone "Europe/Paris")) "dd/MM/yyyy") ;; output -> "27/01/2021"
```

Il existe de nombreuses options pour spécifier le format de sortie de la date, y compris les noms de jours et de mois, les heures, les minutes, les secondes et bien plus encore. Vous pouvez trouver la liste complète des options dans la documentation de `java.text.SimpleDateFormat`, qui est le format utilisé par la fonction `format`.

## Plongée Profonde

Il est important de noter que la fonction `format` renvoie toujours une chaîne de caractères. Si vous souhaitez effectuer des manipulations supplémentaires avec la date, vous devez la convertir en `java.util.Date` en utilisant la fonction `parse`. Voici un exemple de comment convertir une chaîne de caractères en date :

```Clojure
(def date-string "27-01-2021")
(def date (parse "dd-MM-yyyy" date-string)) ;; convertit la chaîne de caractères en date
```

Vous pouvez également utiliser la bibliothèque `clj-time`, qui offre une syntaxe plus concise pour le travail avec les dates en Clojure.

## Voir Aussi

- [Documentation Clojure sur les Dates](https://clojuredocs.org/clojure.core/date)
- [Documentation Java sur SimpleDateFormat](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/text/SimpleDateFormat.html)
- [Documentation pour la bibliothèque clj-time](https://github.com/seejohnrun/clj-time)