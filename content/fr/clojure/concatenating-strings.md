---
title:                "Clojure: Concaténation de chaînes"
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/concatenating-strings.md"
---

{{< edit_this_page >}}

## Pourquoi

Concaténer des chaînes de caractères est une tâche courante en programmation, en particulier lorsque l'on travaille avec des données textuelles. Cela permet de combiner plusieurs chaînes en une seule, ce qui peut être utile dans de nombreuses situations différentes.

## Comment faire

Pour concaténer des chaînes en Clojure, il suffit d'utiliser la fonction `str`. Voici un exemple de code :

```Clojure
(def prenom "Jean")
(def nom "Dupont")
(def nom-complet (str prenom " " nom))
(println nom-complet)
```
Output: "Jean Dupont"

On peut également concaténer plus de deux chaînes en utilisant la même fonction :

```Clojure
(def ville "Paris")
(def code-postal "75001")
(def adresse (str "Adresse : " ville " " code-postal))
(println adresse)
```
Output: "Adresse : Paris 75001"

Il est également possible d'utiliser l'opérateur `str` pour concaténer des chaînes au sein d'une fonction `println` :

```Clojure
(def age 30)
(println "J'ai " (str age) " ans.")
```
Output: "J'ai 30 ans."

## Plongée en profondeur

En Clojure, les chaînes de caractères sont immuables, ce qui signifie qu'elles ne peuvent pas être modifiées une fois créées. Lorsque l'on concatène des chaînes, une nouvelle chaîne est créée chaque fois, ce qui peut provoquer des problèmes de performances si l'on concatène un grand nombre de chaînes.

Pour résoudre ce problème, Clojure propose la fonction `str-join` qui permet de concaténer plusieurs chaînes en une seule opération. Voici un exemple :

```Clojure
(def numeros [1 2 3 4 5])
(println (str-join ", " (map str numeros)))
```
Output: "1, 2, 3, 4, 5"

Cela fonctionne en créant une séquence de chaînes et en les concaténant toutes en une seule fois, ce qui est beaucoup plus efficace en termes de performances.

## Voir aussi

- Documentation officielle Clojure sur la fonction `str` : https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/str
- Documentation officielle Clojure sur la fonction `str-join` : https://clojure.github.io/clojure/clojure.string-api.html#clojure.string/str-join