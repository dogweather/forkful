---
title:    "Clojure: Sortie de débogage d'impression"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Pourquoi

Dans le développement de logiciels, il est souvent nécessaire d'afficher des informations sur le fonctionnement du code pour comprendre les erreurs et les bugs. Cela peut sembler fastidieux à première vue, mais l'impression des sorties de débogage peut grandement faciliter le processus de débogage et de résolution des problèmes.

## Comment faire

L'impression de sorties de débogage en Clojure est très simple grâce à la fonction `println`. Voici un exemple de code qui imprime le message "Hello World" :

```Clojure
(println "Hello World")
```

Et voici le résultat de l'exécution de ce code :

```
Hello World
```

Il existe également d'autres fonctions telles que `pr` et `pprint` qui permettent d'imprimer des objets avec plus de précision et de formatage. Voici un exemple utilisant `pr` pour imprimer une liste :

```Clojure
(pr '(1 2 3))
```

Et voici le résultat :

```
(1 2 3)
```

## Plongée en profondeur

Il est important de noter que l'impression des sorties de débogage doit être utilisée avec parcimonie et être temporaire. Elle ne doit pas être incluse dans le code final car cela peut ralentir l'exécution et rendre le code difficile à lire.

De plus, il est possible d'imprimer des informations à différentes étapes du code pour suivre son déroulement et identifier les erreurs. Il est également possible d'imprimer des expressions ou des valeurs calculées pour vérifier si elles correspondent à ce qui est attendu.

## Voir aussi

- [Documentation officielle de Clojure sur l'impression de débogage](https://clojuredocs.org/clojure.core/println)
- [Article sur l'utilisation de l'impression de débogage en Python](https://realpython.com/python-debugging-pdb/)
- [Tutoriel sur le débogage en Java avec l'impression de sorties](https://www.vogella.com/tutorials/EclipseDebugging/article.html#debugging-introduction)