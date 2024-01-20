---
title:                "Imprimer la sortie de débogage"
html_title:           "Arduino: Imprimer la sortie de débogage"
simple_title:         "Imprimer la sortie de débogage"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/printing-debug-output.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
L'impression des sorties de débogage est un moyen pour les développeurs de vérifier le fonctionnement interne de leur code. C'est un outil précieux pour identifier et corriger les erreurs.

## Comment faire :
Voici un simple exemple de comment imprimer une sortie de débogage en Clojure.

```Clojure
(defn example-debug []
  (let [x 42]
    (println "La valeur de X est: " x)
    (* x 2)))

(example-debug)
```

Lorsque vous exécutez ce code, vous verrez la sortie suivante :

```Clojure
La valeur de X est: 42
```

## Plongée en profondeur
Clojure, un langage fonctionnel dynamique qui cible la JVM, trouve ses origines dans Lisp. L'impression du débogage est un concept simple qui a persisté à travers les générations de langages de programmation grâce à son utilité.

Il existe des alternatives à l'impression pour le débogage, comme l'utilisation d'un débogueur pour inspecter l'état du programme, mais l'impression reste populaire pour sa simplicité.

En Clojure, `println` imprime les arguments, qui sont convertis en chaînes, à la sortie standard et renvoie nil. La fonction est multithread-safe et garantit que les appels concurrents seront correctement sérialisés.

## Voir aussi
1. Documentation officielle de Clojure pour println : [https://clojuredocs.org/clojure.core/println](https://clojuredocs.org/clojure.core/println)