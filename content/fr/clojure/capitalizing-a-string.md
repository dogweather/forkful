---
title:                "Clojure: Capitalisation d'une chaîne de caractères"
simple_title:         "Capitalisation d'une chaîne de caractères"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi
Il y a souvent des cas où l'on veut capitaliser une chaîne de caractères, par exemple pour donner une apparence plus élégante à du texte ou pour respecter les conventions de présentation d'un certain langage. Dans cet article, nous allons apprendre à utiliser Clojure pour capitaliser une chaîne de caractères.

## Comment Faire
La fonction `clojure.string/capitalize` permet de capitaliser la première lettre d'une chaîne de caractères. Elle prend en paramètre la chaîne de caractères à capitaliser et renvoie une nouvelle chaîne de caractères en laquel la première lettre est en majuscule. Voici un exemple d'utilisation:

```Clojure
(clojure.string/capitalize "bonjour")
```
Résultat: "Bonjour"

On peut également utiliser la fonction `clojure.string/upper-case` pour avoir une chaîne de caractères entièrement en majuscules:

```Clojure
(clojure.string/upper-case "bonjour")
```
Résultat: "BONJOUR"

## Plongée en Profondeur
Il est important de noter que la fonction `capitalize` a une option pour la langue qui est utilisée pour déterminer les règles de capitalisation. Si vous travaillez en français, vous devrez définir cette option pour que la fonction fonctionne correctement. Voici un exemple:

```Clojure
(clojure.string/capitalize "école" {:locale "fr"})
```
Résultat: "École"

De plus, la fonction `capitalize` ne modifie pas la chaîne de caractères originale, mais renvoie une nouvelle chaîne de caractères avec la première lettre en majuscule. Si vous voulez modifier directement la chaîne de caractères originale, il faut utiliser la fonction `capitalize!`.

## Voir Aussi
- [Documentation officielle de Clojure sur la fonction capitalize](https://clojuredocs.org/clojure.string/capitalize)
- [Guide pour apprendre à programmer en Clojure](https://clojure.org/guides/learn/syntax)
- [Exemples d'utilisation de la fonction capitalize](https://www.clojuredatascience.com/nlp/named-entity-recognition/)