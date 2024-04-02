---
date: 2024-01-20 17:45:27.693051-07:00
description: "Extraire des sous-cha\xEEnes, c'est comme d\xE9couper un morceau de\
  \ baguette \u2013 on prend juste la partie qu'on veut. Les programmeurs le font\
  \ pour manipuler et\u2026"
lastmod: '2024-03-13T22:44:57.270176-06:00'
model: gpt-4-1106-preview
summary: "Extraire des sous-cha\xEEnes, c'est comme d\xE9couper un morceau de baguette\
  \ \u2013 on prend juste la partie qu'on veut. Les programmeurs le font pour manipuler\
  \ et\u2026"
title: "Extraction de sous-cha\xEEnes"
weight: 6
---

## What & Why?
Extraire des sous-chaînes, c'est comme découper un morceau de baguette – on prend juste la partie qu'on veut. Les programmeurs le font pour manipuler et analyser des données textuelles, simplifier ou localiser l'info.

## How to:
En Clojure, on utilise `subs` pour extraire une sous-chaîne :

```clojure
(let [phrase "La magie de Clojure"]
  (println (subs phrase 3 9))) ; Affiche "magie "
```

`subs` prend la chaîne, le début, et la fin. Facile, non ?

## Deep Dive
Historiquement, Clojure hérite des fonctions de Java, donc `subs` vient de la classe String de Java. Cependant, contrairement à Java, `subs` retourne immédiatement une nouvelle chaîne sans tenir de la chaîne originale. C'est parce que les chaînes en Clojure sont immuables.

Il y a d'autres manières d'obtenir ce qu'on veut. Des expressions régulières avec `re-find` et `re-seq`, ou même des fonctions de plus haut niveau comme `clojure.string/split`. Chaque méthode a sa raison d'être, mais `subs` est souvent le plus direct quand on sait exactement où couper.

En ce qui concerne l'implémentation, `subs` est efficace car les chaînes en Java, et donc en Clojure, sont stockées en interne dans un tableau de caractères. L'opération de sous-chaîne ne nécessite pas de copier l'ensemble du tableau, juste une référence au tableau original avec un début et une fin modifiés.

## See Also
- Clojure `subs`: https://clojuredocs.org/clojure.core/subs
- Utiliser les expressions régulières en Clojure : https://clojuredocs.org/clojure.core/re-find
- `clojure.string` : https://clojure.github.io/clojure/clojure.string-api.html
