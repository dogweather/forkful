---
title:    "Clojure: Majuscule d'une chaîne de caractères"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Pourquoi

Capitaliser une chaîne de caractères peut sembler une tâche simple, mais cela peut être très utile dans le développement de logiciels. En capitalisant une chaîne, vous pouvez la rendre plus visible et faciliter son utilisation dans des fonctions telles que la recherche et le tri de données.

## Comment faire

```Clojure
(defn capitalize-string [str]
  (clojure.string/capitalize str))

(def input "bonjour")
(def output (capitalize-string input))
```

Le code ci-dessus utilise la fonction `capitalize` de la bibliothèque standard Clojure pour capitaliser la chaîne de caractères `input`. Ensuite, le résultat `output` est stocké dans une nouvelle variable. 

```
Output: "Bonjour"
```

Dans cet exemple, nous avons utilisé la fonction `capitalize` de base, mais il existe également d'autres façons de capitaliser une chaîne. Par exemple, vous pouvez utiliser les fonctions `upper-case` et `lower-case` pour changer la casse d'une chaîne en majuscules et en minuscules respectivement.

```Clojure
(defn alternate-capitalize-string [str]
  (str/upper-case str))

(defn alternate-capitalize-string2 [str]
  (str/lower-case str))

(def input2 "bonjour")
(def output2 (alternate-capitalize-string input2))
(def output3 (alternate-capitalize-string2 input2))
```

```
Output 2: "BONJOUR"
Output 3: "bonjour"
```

## Plongée en profondeur

En utilisant la fonction `capitalize` et d'autres fonctions telles que `replace` et `interpose`, vous pouvez également capitaliser des chaînes avec des règles spécifiques, comme le fait de capitaliser la première lettre de chaque mot ou même d'inverser la casse de chaque caractère.

Par exemple, vous pouvez utiliser `replace` pour remplacer la première lettre de chaque mot par sa version capitale, et utiliser `interpose` pour insérer un espace entre chaque mot.

```Clojure
(defn capitalize-words [str]
  (str/replace (str/lower-case str) #"(\w)([\w-]*)|\W#" (fn [[_ f r :as g]] (when f (str/upper-case f) r))))

(def input3 "bOnJOur mON aMI")
(def output4 (str/interpose " " (capitalize-words input3)))
```

```
Output 4: "Bonjour Mon Ami"
```

Cela peut être utile lorsque vous avez besoin de capitaliser des titres ou des noms de personnes.

## Voir aussi

- Documentation officielle de la fonction `capitalize`: https://clojuredocs.org/clojure.string/capitalize
- Liste de toutes les fonctions de manipulation de chaînes de caractères en Clojure: https://clojuredocs.org/quickref#strings