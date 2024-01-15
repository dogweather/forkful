---
title:                "Affichage du débogage"
html_title:           "Clojure: Affichage du débogage"
simple_title:         "Affichage du débogage"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/printing-debug-output.md"
---

{{< edit_this_page >}}

## Pourquoi

La sortie de débogage est un outil essentiel pour comprendre le fonctionnement de votre code et résoudre les problèmes. En imprimant des informations sur l'état de votre programme à différents points d'exécution, vous pouvez identifier les erreurs et suivre le parcours de vos données.

## Comment faire

```Clojure
(defn afficher-statut [statut]
  (println "Le statut actuel est :" statut))
  
(defn operation-complexe [a b]
  (print "Effectuer une opération complexe avec les valeurs :" a b)
  (+ a b))
```

L'exemple ci-dessus montre comment imprimer des informations sur des variables et des expressions dans votre code. En utilisant `println` et `print`, vous pouvez afficher la valeur actuelle des variables ainsi que des messages pour aider à comprendre le contexte de l'exécution du code.

Voici un exemple de sortie de débogage pour la fonction `operation-complexe` avec une valeur de `a` de 2 et une valeur de `b` de 3 :

```
Effectuer une opération complexe avec les valeurs : 2 3
Le résultat est : 5
```

## Plongée en profondeur

L'impression de sortie de débogage peut être personnalisée en utilisant des options telles que `pr`, `prn` et `pprint`, qui offrent différentes façons de formater les données pour une lisibilité maximale. Vous pouvez également utiliser des bibliothèques telles que `clojure.tools.logging` pour une meilleure gestion de l'output de débogage.

Voici un exemple de sortie de débogage avec la fonction `pr` :

```Clojure
(defn afficher-nombre [nombre]
  (pr "Le type de nombre est : " (type nombre))
  (pr "La valeur de nombre est : " nombre))
  
(afficher-nombre 5)
```

La sortie sera :

```
Le type de nombre est : java.lang.Long
La valeur de nombre est : 5
```

## Voir aussi

- [Documentation officielle de Clojure](https://clojure.org/)
- [Tutoriels Clojure de Clojure for the Brave and True](https://www.braveclojure.com/programming/)
- [Exemples de code sur GitHub](https://github.com/clojure/clojure)