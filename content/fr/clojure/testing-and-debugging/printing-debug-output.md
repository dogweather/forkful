---
date: 2024-01-20 17:52:16.766141-07:00
description: "L'impression de d\xE9bogage, c'est \xE9crire des r\xE9sultats temporaires\
  \ pour voir ce qui se passe dans votre code. Les d\xE9veloppeurs le font pour identifier\
  \ les\u2026"
lastmod: '2024-03-13T22:44:57.284831-06:00'
model: gpt-4-1106-preview
summary: "L'impression de d\xE9bogage, c'est \xE9crire des r\xE9sultats temporaires\
  \ pour voir ce qui se passe dans votre code."
title: "Affichage des sorties de d\xE9bogage"
weight: 33
---

## What & Why? (Quoi & Pourquoi ?)
L'impression de débogage, c'est écrire des résultats temporaires pour voir ce qui se passe dans votre code. Les développeurs le font pour identifier les bugs ou vérifier le flux d'exécution.

## How to: (Comment faire :)

Pour imprimer des messages de débogage en Clojure, `println` est votre ami. Voyons quelques exemples :

```Clojure
;; Imprime un message simple
(println "Voici un message de débogage")

;; Imprime des variables et des expressions
(let [x 42]
  (println "La valeur de x est :" x))

;; Imprime avec formatage
(printf "Les valeurs sont : %d et %s\n" 42 "Quarante-deux")
```

Sortie :
```
Voici un message de débogage
La valeur de x est : 42
Les valeurs sont : 42 et Quarante-deux
```

## Deep Dive (Plongée en profondeur)

Historiquement, `println` et `printf` viennent de Java, la plateforme sur laquelle Clojure est construite. Un alternative pour le débogage en Clojure est d'utiliser des outils comme `clojure.tools.logging` ou des IDEs qui offrent des capacités de débogage intégrées. Pour imprimer des données structurées, `prn` est plus approprié car il conserve le format des données de Clojure. N'utilisez l'impression de débogage que quand c'est nécessaire, car trop de sorties console peuvent ralentir votre application.

## See Also (Voir aussi)

- [Clojure Documentation](https://clojure.org/guides/getting_started)
- [clojure.tools.logging](https://github.com/clojure/tools.logging)
- [Clojure Style Guide](https://guide.clojure.style/#print-debugging)
