---
title:    "Clojure: Écrire sur l'erreur standard"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Pourquoi

Écrire sur une sortie d'erreur standard (standard error output) peut être utile dans le processus de débogage en affichant des informations d'erreur en temps réel.

## Comment faire

```Clojure 
;; Importer la bibliothèque 'clojure.java.io' pour accéder à la fonction
;; pour écrire sur la sortie d'erreur standard
(require '[clojure.java.io :as io])

;; Utiliser 'with-out-str' pour capturer ce qui est écrit dans la sortie
;; d'erreur standard dans une chaîne
(with-out-str
  (println "Voici un message d'erreur."))

;; Afficher la chaîne capturée en utilisant la fonction 'err' de 'io'
(println (io/err))

;; Vous pouvez également écrire directement sur la sortie d'erreur standard
;; en utilisant 'println' et 'System/err'
(println "Ceci est un autre message d'erreur." System/err)
```

Output:
```
Voici un message d'erreur.
Ceci est un autre message d'erreur.
```

## Une plongée en profondeur

Outre l'utilisation principale pour le débogage, écrire sur la sortie d'erreur standard peut également être utile pour afficher des informations de diagnostic ou pour des tests. Il est important de noter que les messages écrits sur la sortie d'erreur standard sont généralement visibles par l'utilisateur final, il est donc essentiel de les utiliser avec parcimonie et de les supprimer une fois qu'ils ont servi leur but.

## Voir aussi

- [Documentation officielle de Clojure pour la bibliothèque 'clojure.java.io'](https://clojure.github.io/clojure/clojure.java.io-api.html)
- [Article sur l'utilisation de 'with-out-str' en Clojure](https://danielcompton.net/2011/08/06/ouput-capture-in-clojure)