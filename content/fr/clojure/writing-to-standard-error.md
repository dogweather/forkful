---
title:                "Écrire vers la sortie d'erreur standard"
html_title:           "Clojure: Écrire vers la sortie d'erreur standard"
simple_title:         "Écrire vers la sortie d'erreur standard"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Pourquoi

Ecrire vers le flux d'erreur standard est un moyen pratique de déboguer votre code. Cela vous permet de voir les erreurs qui se produisent pendant l'exécution de votre programme, ce qui est essentiel pour identifier et résoudre les problèmes.

## Comment faire

Pour écrire vers le flux d'erreur standard en Clojure, utilisez la fonction `println-multiline` en lui passant en paramètre une chaîne de caractères. Par exemple :

```Clojure
(println-multiline "Ceci est une erreur")
```
Cela écrira la chaîne de caractères `"Ceci est une erreur"` vers le flux d'erreur standard, qui sera ensuite affichée dans la console lors de l'exécution du programme.

Si vous voulez écrire un objet plus complexe vers le flux d'erreur standard, vous pouvez utiliser la fonction `prn`en lui passant en paramètre l'objet que vous souhaitez afficher. Par exemple :

```Clojure
(def error {:type "Erreur de validation" :message "La valeur entrée n'est pas valide"})

(prn error)
```

Cela écrira l'objet `error` vers le flux d'erreur standard, qui sera ensuite affiché sous la forme d'une liste dans la console.

## Plongée en profondeur

La différence entre les fonctions `println-multiline` et `prn` est que `println-multiline` affiche la chaîne de caractères passée en paramètre telle quelle, tandis que `prn` affiche une version lisible de l'objet. Cela peut être utile lorsque vous essayez de comprendre la structure d'un objet ou d'un ensemble de données complexes.

Il est également bon de noter que le flux d'erreur standard est généralement réservé pour les erreurs et les avertissements importants. Pour les messages de débogage ou de journalisation, il est préférable d'utiliser le flux de sortie standard avec la fonction `println`.

## Voir aussi

- Documentation officielle de Clojure sur l'utilisation des flux d'erreur : https://clojuredocs.org/clojure.core/with-err-out
- Un article de blog sur la gestion des erreurs en Clojure : https://purelyfunctional.tv/article/error-handling-in-clojure/