---
title:                "Clojure: Utiliser les expressions régulières"
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Pourquoi
Les expressions régulières sont un outil puissant et polyvalent pour la manipulation de chaînes de caractères en Clojure. En utilisant des motifs spécifiques, elles permettent de rechercher, de valider et de remplacer des chaînes de caractères de manière rapide et efficace. Que vous soyez un débutant en programmation ou un développeur expérimenté, les expressions régulières peuvent grandement améliorer votre expérience de codage en Clojure.

## Comment faire
Les expressions régulières en Clojure sont définies avec le mot-clé `#"pattern"` et peuvent être utilisées avec la fonction `re-seq`. Voici un exemple simple pour trouver tous les nombres dans une chaîne de caractères :

```Clojure
(re-seq #"[\d]+" "Il y a 5 pommes dans le panier.")
```
La sortie sera `("5")`, car `[d]` est un raccourci pour `[0-9]`, qui correspond à un seul chiffre. Vous pouvez également utiliser des opérateurs tels que `+` pour correspondre à un ou plusieurs éléments, `*` pour correspondre à zéro ou plusieurs éléments, et `?` pour correspondre à zéro ou un élément.

Pour valider une chaîne de caractères, vous pouvez utiliser la fonction `re-matches`, qui renvoie `true` si la chaîne correspond au motif donné et `false` sinon. Par exemple :

```Clojure
(let [pattern #"Date : \d{2}/\d{2}/\d{4}"
      string "Date : 01/01/2021"]
  (re-matches pattern string))
```
La sortie sera `true`, car `d{2}` correspond à deux chiffres et `d{4}` correspond à quatre chiffres.

Enfin, pour remplacer une partie d'une chaîne de caractères, vous pouvez utiliser la fonction `re-sub` et spécifier le motif à remplacer et le remplacement souhaité. Par exemple :

```Clojure
(re-sub #"pommes" "bananes" "Il y a 5 pommes dans le panier.")
```
La sortie sera `"Il y a 5 bananes dans le panier."`.

## Plongée en profondeur
Il existe de nombreux modèles et opérateurs que vous pouvez utiliser pour créer des expressions régulières complexes et puissantes. Avec la pratique et la compréhension de la syntaxe des expressions régulières, vous pouvez les utiliser pour résoudre des problèmes de manipulation de chaînes de caractères beaucoup plus rapidement et facilement qu'avec des fonctions de base en Clojure.

Il est également important de noter que les expressions régulières peuvent être sensibles à la casse et qu'il existe des options telles que `re-matches?i` pour les rendre insensibles à la casse. Vous pouvez également utiliser des groupes et des références de groupes pour capturer et réutiliser des parties spécifiques des correspondances.

## Voir aussi
- [Documentation officielle de la classe `re-matches`](https://clojuredocs.org/clojure.core/re-matches)
- [Guide des expressions régulières en Clojure](https://clojure.org/reference/regexp)
- [Référence interactive pour les expressions régulières en Clojure](https://regexone.com/references/clojure)