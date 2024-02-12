---
title:                "Trouver la longueur d'une chaîne de caractères"
aliases:
- fr/clojure/finding-the-length-of-a-string.md
date:                  2024-01-20T17:47:17.128551-07:00
model:                 gpt-4-1106-preview
simple_title:         "Trouver la longueur d'une chaîne de caractères"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Trouver la longueur d'une chaîne de caractères, c'est compter le nombre d'éléments (caractères) qui la composent. Les programmeurs le font pour valider des entrées, manipuler du texte ou limiter la taille des données.

## How to:
En Clojure, on utilise la fonction `count` pour obtenir la longueur d'une chaîne de caractères :

```Clojure
(count "Bonjour")
```

Sortie attendue :

```Clojure
7
```

Un autre exemple avec une chaîne de caractères vide :

```Clojure
(count "")
```

Sortie attendue :

```Clojure
0
```

## Deep Dive
Historiquement, la nécessité de connaître la longueur d'une chaîne est liée à la gestion de la mémoire et aux opérations sur les données textuelles. En Clojure, et dans les langues LISP en général, la simplicité prime donc `count` fait le travail. Alternativement, en Java (la plateforme sur laquelle Clojure s'exécute), on utiliserait la méthode `.length()` sur un objet `String`.

Pour des raisons de performance, `count` est aussi efficace que possible avec les différents types de séquences en Clojure. Cette efficacité est importante quand on manipule de grandes collections de données.

## See Also
Pour plus d’informations :

- ClojureDocs pour des exemples communautaires : [clojuredocs.org/clojure.core/count](https://clojuredocs.org/clojure.core/count)
