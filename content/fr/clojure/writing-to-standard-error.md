---
title:                "Clojure: Écrire vers l'erreur standard"
programming_language: "Clojure"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Pourquoi

L'écriture sur la sortie standard d'erreur, également appelée stderr, peut être utile lors du débogage de programmes. Elle permet aux développeurs de suivre les erreurs et les problèmes de leur code en temps réel. Cela peut également aider à identifier les parties du code qui nécessitent une attention particulière.

## Comment faire

Dans Clojure, l'écriture sur stderr peut être réalisée avec la fonction `println` combinée avec la fonction `System/err`. Par exemple, pour imprimer un message sur stderr, nous pouvons utiliser le code suivant :

```Clojure
(println "Il y a une erreur dans le code." (System/err))
```

Ce code imprimera le message "Il y a une erreur dans le code." suivi de la sortie standard d'erreur. En utilisant cette méthode, nous pouvons facilement suivre les erreurs et les problèmes dans notre code.

## Plongée en profondeur

Il est important de noter que l'écriture sur la sortie standard d'erreur ne doit être utilisée que pour le débogage et le suivi des erreurs. Elle ne doit pas être utilisée comme moyen de communication avec l'utilisateur final. Pour cela, nous devons utiliser la sortie standard, également appelée stdout.

De plus, il est possible d'utiliser la fonction `System/err` pour gérer les exceptions dans notre code. Par exemple, nous pouvons utiliser `System/err` pour imprimer une trace d'erreur personnalisée si une exception est levée dans notre code.

## Voir aussi

- [Documentation officielle de Clojure sur la sortie standard d'erreur](https://clojuredocs.org/clojure.core/printerr)
- [Article sur la gestion des erreurs en Clojure](https://purelyfunctional.tv/article/handling-errors-in-clojure/)