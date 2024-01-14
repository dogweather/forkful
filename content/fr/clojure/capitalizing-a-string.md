---
title:                "Clojure: Capitalisation d'une chaîne de caractères"
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

La capitalisation d'une chaîne de caractères est une tâche très courante en programmation, et elle peut être utile pour de nombreuses raisons telles que l'affichage de données à l'utilisateur, la manipulation de données dans des bases de données, ou encore pour des fins de validation de données.

## Comment faire

Pour capitaliser une chaîne de caractères en Clojure, vous pouvez utiliser la fonction `clojure.string/capitalize`. Cette fonction prend en paramètre une chaîne de caractères et renvoie une nouvelle chaîne avec la première lettre en majuscule et toutes les autres en minuscule.

Voici un exemple de code :

```
(clojure.string/capitalize "bonjour") ; renvoie "Bonjour"
```

Vous pouvez également utiliser cette fonction pour capitaliser seulement la première lettre d'une phrase, en utilisant la fonction `clojure.string/join` pour recoller la chaîne à son reste comme ceci :

```
(clojure.string/join " " (map clojure.string/capitalize (.split "bonjour tout le monde" #"\s+"))) ; renvoie "Bonjour Tout Le Monde"
```

## Plongée en profondeur

Il est important de noter que la fonction `capitalize` utilise les règles de capitalisation de l'Unicode. Cela signifie que si une lettre n'est pas définie dans l'ensemble de caractères Unicode, elle ne sera pas modifiée. De plus, si une lettre a plusieurs versions en minuscule ou en majuscule dans l'Unicode, la première version sera utilisée.

Pour une utilisation plus avancée, vous pouvez également utiliser la fonction `clojure.string/lower-case` pour capitaliser une chaîne en minuscule ou `clojure.string/upper-case` pour la capitaliser en majuscule.

## Voir aussi

- [Documentation officielle de Clojure pour la fonction `capitalize`](https://clojuredocs.org/clojure.string/capitalize)
- [Liste des caractères Unicode](https://unicode.org/charts/)
- [Autres fonctions utiles pour manipuler les chaînes en Clojure](https://thoughtbot.com/playbook/clojure/strings)

Merci d'avoir lu cet article sur la capitalisation de chaînes en Clojure. Nous espérons que cela vous aidera à mieux comprendre cette fonction et à l'utiliser dans vos projets futurs. À bientôt !