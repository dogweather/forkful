---
title:    "Clojure: Trouver la longueur d'une chaîne de caractères"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

# Pourquoi
La longueur d'une chaîne est une mesure très utile en programmation. Elle peut nous aider à manipuler et à traiter des chaînes de caractères de manière plus efficace. Dans cet article, nous allons explorer comment trouver la longueur d'une chaîne en utilisant Clojure.

# Comment Faire
Tout d'abord, nous devons comprendre que les chaînes de caractères sont représentées comme des séquences de caractères en Clojure. Cela signifie que nous pouvons utiliser des fonctions telles que `count` pour trouver la longueur d'une chaîne.

```
Clojure
(count "Bonjour")
```
La sortie de ce code sera `7` car la chaîne "Bonjour" contient 7 caractères.

Une autre façon de trouver la longueur d'une chaîne est d'utiliser la fonction `length`. Cette fonction est spécifique aux chaînes de caractères et nous donne le même résultat que `count`.

```
Clojure
(length "Bonjour")
```
Encore une fois, la sortie sera `7`.

# Plongée en Profondeur
Il est important de noter que la longueur d'une chaîne est différente de son index. Par exemple, pour la chaîne "Bonjour", la longueur est de 7 mais l'index du dernier caractère est de 6. Cela est dû au fait que les indices commencent à 0 en Clojure.

De plus, la fonction `count` fonctionne également sur d'autres types de données en plus des chaînes de caractères. Elle peut être utilisée sur des listes, des vecteurs, des ensembles, etc. pour trouver la taille de ces structures de données.

# Voir Aussi
- Documentation Clojure sur les chaînes de caractères : https://clojuredocs.org/clojure.string
- Tutoriel sur les structures de données en Clojure : https://lispcast.com/learn-clojure-data-structures/