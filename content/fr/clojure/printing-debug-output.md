---
title:                "Affichage de données de débogage"
html_title:           "Clojure: Affichage de données de débogage"
simple_title:         "Affichage de données de débogage"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/printing-debug-output.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

L'impression de Debug Output est simplement le fait d'afficher des informations pendant l'exécution de votre code, principalement pour résoudre les bugs et trouver des erreurs.

Les programmeurs le font souvent pour comprendre exactement ce qui se passe dans leur code et repérer les problèmes plus facilement.

## Comment:

```clojure 
(defn print-debug [var]
  (println "Debug Output:" var))

(print-debug "Hello world!")
```

Résultat:

```
Debug Output: Hello world!
```

Vous pouvez également utiliser ```prn``` à la place de ```println``` pour obtenir une représentation imprimable de l'objet.

## Plongée en profondeur:

L'impression de Debug Output a été une technique courante pour le débogage depuis les débuts de la programmation informatique. Cependant, il existe maintenant des alternatives telles que l'utilisation d'un débogueur ou d'un logger.

En utilisant ```println``` ou ```prn```, le débogueur peut influencer les performances de votre code et il est important de le désactiver lorsque vous envoyez votre code en production.

## Voir aussi:

Pour en savoir plus sur l'impression de Debug Output en Clojure, vous pouvez consulter les sources suivantes:

- [Documentation officielle de Clojure sur l'impression de Debug Output](https://clojuredocs.org/clojure.pprint/pprint)
- [Vidéo explicative d'utilisation de ```println``` en Clojure](https://www.youtube.com/watch?v=61VheNAfQVQ)
- [Article sur les meilleurs outils de débogage en Clojure](https://blog.lono.io/best-clojure-debugging-tools/)