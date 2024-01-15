---
title:                "Écriture de tests"
html_title:           "Clojure: Écriture de tests"
simple_title:         "Écriture de tests"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/writing-tests.md"
---

{{< edit_this_page >}}

## Pourquoi
Ecrire des tests peut sembler fastidieux et inutile, mais c'est en réalité un aspect crucial de la programmation. Les tests garantissent la stabilité et la fiabilité de votre code, ce qui vous permet de détecter et de résoudre les problèmes avant qu'ils ne deviennent des bugs dans votre code.

## Comment Faire
Pour écrire des tests en Clojure, vous devez d'abord inclure la bibliothèque de tests dans votre fichier. Cela se fait en ajoutant la ligne suivante en haut de votre fichier :
```
(ns mon-projet.tests (:require [clojure.test :refer :all]))
```
Ensuite, vous pouvez définir vos tests en utilisant le macro `deftest`. Par exemple, si vous voulez tester une fonction `addition` qui prend deux nombres en entrée et renvoie leur somme, vous pouvez écrire :
```
(deftest test-addition
  (is (= (addition 2 3) 5))
  (is (= (addition 5 -3) 2)))
```
Dans cet exemple, nous utilisons la fonction `is` pour vérifier si l'expression donnée est vraie. Si les expressions sont valides, le test réussit, sinon il échoue. Enfin, vous pouvez exécuter vos tests en appelant `run-tests` à la fin de votre fichier :
```
(run-tests)
```
Vous obtiendrez une liste des tests exécutés et leur statut (passé ou échoué).

## Plongée Profonde
Il y a quelques astuces à garder à l'esprit lors de l'écriture de tests en Clojure :

- Utilisez des données de test variées pour couvrir autant de cas de figure que possible.
- Utilisez les fonctions `is` et `is-not` pour vérifier si les expressions sont vraies ou fausses respectivement.
- Utilisez les fonctions `=`, `=`, `==`, `not=` et `nil?` pour les comparaisons de valeurs.
- Utilisez les fonctions `:throws` et `thrown` pour vérifier les exceptions levées par votre code.
- Utilisez les fonctions `testing` et `use-fixtures` pour organiser et gérer vos tests de manière plus efficace.

## Voir Aussi
- [Documentation officielle de tests en Clojure](https://clojure.org/guides/learn/testing)
- [Clojure Cookbook : écrire des tests en Clojure](https://lispcast.com/writing-clojure-tests)
- [Introduction aux tests en Clojure](https://medium.com/jostle-engineering/clojure-unit-testing-basics-47279b540ce7)