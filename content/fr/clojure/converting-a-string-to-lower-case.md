---
title:    "Clojure: Passage d'une chaîne de caractères en minuscules"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Pourquoi

La conversion d'une chaîne de caractères en minuscules peut être très utile dans diverses situations en programmation Clojure. Cela peut faciliter la recherche et la comparaison de chaînes de caractères, ainsi que la manipulation de données saisies par les utilisateurs.

## Comment faire

```Clojure
(let [texte "Ceci est un TEXTE en majuscules."]
  (clojure.string/lower-case texte))
```
```Clojure
Résultat: "ceci est un texte en majuscules."
```

Dans cet exemple, nous créons une variable contenant une chaîne de caractères en majuscules, puis nous appelons la fonction `lower-case` du module `clojure.string` pour la convertir en minuscules. Cette fonction prend en paramètre la chaîne de caractères à convertir et renvoie une nouvelle chaîne en minuscules.

Il est également possible d'utiliser la fonction `str/lower-case` du module standard `clojure.core` pour convertir une chaîne de caractères en minuscules. La différence est que cette fonction peut prendre en paramètre plusieurs chaînes de caractères à convertir en une seule fois.

```Clojure
(str/lower-case "Ceci est" "un TEXTE" "en majuscules.")
```
```Clojure
Résultat: "ceci est un texte en majuscules."
```

## Plongeon en profondeur

La conversion d'une chaîne de caractères en minuscules peut sembler simple, mais il est important de comprendre la différence entre majuscules et minuscules, notamment en ce qui concerne les caractères accentués.

En programmation Clojure, les caractères accentués sont représentés par des séquences Unicode appelées "combining characters". Lors de la conversion en minuscules, ces séquences sont traitées de manière différente selon les différentes fonctions utilisées. Par exemple, `lower-case` ignore ces séquences, tandis que `str/lower-case` les convertit en une seule fois.

Il est donc important de choisir la fonction de conversion en minuscules en fonction de vos besoins et de comprendre comment elle gère ces caractères.

## Voir aussi

- Documentation officielle sur `clojure.string/lower-case` : https://clojure.github.io/clojure/clojure.string-api.html#clojure.string/lower-case
- Documentation officielle sur `clojure.core/str/lower-case` : https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/str
- Article sur la différence entre `lower-case` et `str/lower-case` : https://clojureverse.org/t/difference-between-lower-case-and-clojure-string-lower-case/1241