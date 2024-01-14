---
title:                "Clojure: Afficher les messages de débogage"
simple_title:         "Afficher les messages de débogage"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/printing-debug-output.md"
---

{{< edit_this_page >}}

## Pourquoi 

Imaginons que vous travailliez sur un projet Clojure et que vous rencontriez un problème avec une partie de votre code. Vous avez identifié la section du code où le problème se produit, mais vous ne savez pas exactement quelles valeurs sont utilisées à ce moment-là. C'est là que l'impression de sortie de débogage entre en jeu. En imprimant les valeurs des variables ou des expressions à un moment précis dans votre code, vous pouvez mieux comprendre ce qui se passe et résoudre votre problème plus rapidement.

## Comment faire 

L'impression de sortie de débogage en Clojure est très simple. Vous pouvez utiliser la fonction `println` pour afficher une valeur ou un message spécifique dans la console. Par exemple : 

```Clojure
(def name "Jean")
(println "Bonjour" name)

;; Output: Bonjour Jean
```

Vous pouvez également afficher plusieurs valeurs en les séparant avec une virgule : 

```Clojure
(def age 30)
(def country "France")
(println "Je suis" name "et j'ai" age "ans. Je viens de" country)

;; Output: Je suis Jean et j'ai 30 ans. Je viens de France
```

En utilisant une combinaison de `println` et de la fonction `str`, vous pouvez même afficher des valeurs à l'intérieur d'une chaîne de caractères : 

```Clojure
(def num1 10)
(def num2 20)
(println (str "La somme de " num1 " et " num2 " est " (+ num1 num2)))

;; Output: La somme de 10 et 20 est 30
```

## Plongée approfondie 

En plus de `println`, il existe d'autres fonctions et outils utiles pour l'impression de sortie de débogage en Clojure. La bibliothèque `clojure.pprint` offre des fonctions telles que `pprint` et `pprint-table` pour imprimer des valeurs de manière plus lisible. Vous pouvez également utiliser la macro `clojure.pprint/pprint` pour limiter la profondeur d'impression et éviter d'afficher des valeurs trop complexes.

En outre, certaines intégrations de développement telles que Cider pour Emacs et Calva pour VS Code offrent des fonctionnalités de débogage avancées, y compris l'impression de variables en direct dans votre éditeur.

## Voir aussi 

- [Documentation officielle de Clojure sur l'impression de sortie de débogage](https://clojure.org/guides/repl/debugging)
- [Tutoriel de Clojure sur l'impression de sortie de débogage](https://clojure.org/guides/repl/debugging_with_clojure_repl)
- [Vidéo de formation sur l'impression de sortie de débogage en Clojure](https://www.youtube.com/watch?v=yIMCw7s7pjE)