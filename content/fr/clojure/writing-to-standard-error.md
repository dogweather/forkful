---
title:                "Écrire dans l'erreur standard"
date:                  2024-01-19
html_title:           "Arduino: Écrire dans l'erreur standard"
simple_title:         "Écrire dans l'erreur standard"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?
Écrire sur l'erreur standard (`stderr`) permet de séparer les messages d'erreur des sorties normales du programme (`stdout`). Les programmeurs font cela pour diagnostiquer les problèmes sans perturber le flux de données principal du programme.

## Comment faire :
```Clojure
; Afficher du texte sur stdout
(println "Ceci est un message sur le flux de sortie standard (stdout).")

; Afficher du texte sur stderr
(binding [*err* *out*]
  (println "Ceci est un message sur le flux d'erreur standard (stderr)."))
```
Sortie attendue sur `stderr` :
```
Ceci est un message sur le flux d'erreur standard (stderr).
```

## Plongée en profondeur
Historiquement, la distinction entre `stdout` et `stderr` provient des premiers terminaux Unix. `stdout` est utilisé pour les données de sortie "normales" tandis que `stderr` est réservé aux messages d’erreur ou de diagnostic. Alternativement, on peut écrire sur `stderr` avec Java interop, mais en Clojure, `binding` est plus idiomatique. La redirection de `*err*` vers `*out*`, comme indiqué dans l'exemple, change la destination de la sortie d'erreur pour le corps du `binding` seulement.

## Voir également
- [Documentation officielle Clojure sur le flux d'erreur standard](https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/*err*)
- [Comparaison des flux stdout et stderr](https://unix.stackexchange.com/questions/331611/do-stdout-and-stderr-need-to-be-explicitly-closed)
