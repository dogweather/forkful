---
date: 2024-01-20 17:50:24.847191-07:00
description: "Comment faire : Historiquement, Clojure, tout comme sa famille Lisp,\
  \ n'a pas eu une fonction d'interpolation de cha\xEEnes int\xE9gr\xE9e comme dans\
  \ d'autres\u2026"
lastmod: '2024-04-05T21:53:58.847822-06:00'
model: gpt-4-1106-preview
summary: "Historiquement, Clojure, tout comme sa famille Lisp, n'a pas eu une fonction\
  \ d'interpolation de cha\xEEnes int\xE9gr\xE9e comme dans d'autres langages modernes."
title: "Interpolation de cha\xEEnes de caract\xE8res"
weight: 8
---

## Comment faire :
```Clojure
;; Utiliser `str` pour concaténer des chaînes et des variables
(def name "Mundo")
(println (str "Hola, " name "!")) ;; Output: Hola, Mundo!

;; Utiliser `format` pour plus de contrôle dans la substitution
(def age 30)
(println (format "J'ai %d ans." age)) ;; Output: J'ai 30 ans.
```

## Plongée en profondeur
Historiquement, Clojure, tout comme sa famille Lisp, n'a pas eu une fonction d'interpolation de chaînes intégrée comme dans d'autres langages modernes. Pour combler ce vide, on utilise souvent `str` ou `format`. `str` est plus simple, alors que `format` offre un contrôle de formatage semblable à `printf` en C.

Il existe des alternatives via des bibliothèques tierces comme `clojure.string/interpolate` ou des utilitaires personnalisés, qui peuvent rendre l'interpolation plus directe et élégante.

Du point de vue de l'implémentation, l'interpolation consiste à évaluer des expressions ou des variables et à les transformer en chaînes avant de les assembler. Cela doit être fait de manière sûre pour éviter les injections de code malveillantes.

## Voir également
- ClojureDocs on `str`: https://clojuredocs.org/clojure.core/str
- ClojureDocs on `format`: https://clojuredocs.org/clojure.core/format
- Un article sur l'interpolation de chaînes dans Clojure : https://clojurebyexample.com/examples/string-interpolation
