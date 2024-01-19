---
title:                "Trouver la longueur d'une chaîne"
html_title:           "Go: Trouver la longueur d'une chaîne"
simple_title:         "Trouver la longueur d'une chaîne"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
La longueur d'une chaîne fait référence au nombre de caractères qu'elle contient. Les programmeurs s'occupent de cela pour gérer efficacement l'espace mémoire et pour manipuler les chaînes de manière plus précise.

## Comment faire :
Voici comment vous pouvez trouver la longueur d'une chaîne en Clojure:

```Clojure 
(defn longueur-chaine [chaine]
  (count chaine))
```

Testons maintenant cela en entrant une chaîne de caractères :

```Clojure 
(longueur-chaine "Bonjour!")
```

Le résultat sera :

```Clojure 
8
```
Donc, la chaîne "Bonjour!" contient 8 caractères.

## Plongeon Profond :
Clojure, comme toutes les langues lispiennes, utilise la fonction 'count' pour obtenir la longueur d'une séquence. Cependant, cette fonction retourne la longueur du caractère et non pas le nombre de bytes. Si on veut obtenir le nombre de bytes, on peut utiliser la méthode `.getBytes` disponible sur les objets String de Java. 

Alternativement, on peut aussi utiliser la fonction 'length' de l'API Java String pour obtenir la longueur d'une chaîne:

```Clojure
(.length "Bonjour!")
```

Clojure est construit sur la JVM (Java Virtual Machine) et peut donc utiliser les méthodes des classes Java sans problème.

## Voir Aussi :
- [Documentation officielle de Clojure 'count'](https://clojuredocs.org/clojure.core/count)
- [API Java String 'length'](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html#length())
- [Guide de programmation Clojure de Clojure.org](https://clojure.org/guides/getting_started)
- [Introduction à Clojure - Sequences](https://clojure.org/guides/learn/sequences)