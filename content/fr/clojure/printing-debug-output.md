---
title:                "Clojure: Affichage de sortie de débogage"
programming_language: "Clojure"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/printing-debug-output.md"
---

{{< edit_this_page >}}

## Pourquoi

Il arrive souvent qu'en tant que programmeurs, nous ayons besoin de comprendre comment notre code est en train d'être exécuté. Imprimer des sorties de débogage est un moyen simple et efficace de suivre le déroulement de notre code et de déceler toute erreur ou problème potentiel.

## Comment faire

Pour imprimer une sortie de débogage en Clojure, nous pouvons utiliser la fonction `println` et y passer en argument le message que nous souhaitons afficher. Par exemple:

```Clojure
(defn somme [a b]
  (println "Calcul de la somme...")
  (+ a b))
```
Cette fonction `somme` va imprimer "Calcul de la somme..." avant de calculer et renvoyer la somme des deux valeurs `a` et `b`.

## Approfondissement

Imprimer des sorties de débogage peut également nous aider à comprendre le flux de données et les valeurs des variables à travers notre code. Par exemple, nous pouvons utiliser la méthode `prn` pour imprimer une représentation plus détaillée de ces valeurs. De plus, en utilisant la macro `with-out-str`, nous pouvons stocker ces sorties de débogage dans une chaîne de caractères pour un traitement ultérieur.

## Voir aussi

- [Documentation officielle de Clojure sur l'impression de sorties de débogage](https://clojuredocs.org/clojure.core/println)
- [Article de blog "Debugging Tools in Clojure"](https://half-countplus7.com/posts/clojure-debugging-tools/)
- [Vidéo "Debugging in Clojure" par Eric Normand](https://www.youtube.com/watch?v=U5xHFlsGIZ0)