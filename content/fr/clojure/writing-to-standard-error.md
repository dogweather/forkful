---
title:                "Écrire sur la sortie d'erreur standard"
html_title:           "Clojure: Écrire sur la sortie d'erreur standard"
simple_title:         "Écrire sur la sortie d'erreur standard"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Qu'est-ce et pourquoi ?

Ecrire sur la sortie standard d'erreur est une façon pour les programmeurs de communiquer des messages d'erreurs ou de débogage directement à l'utilisateur du programme. Cela peut être utile pour signaler des problèmes lors de l'exécution du code, tels que des erreurs de syntaxe ou des valeurs incorrectes.

## Comment faire :

Voici un exemple de code en Clojure montrant comment écrire sur la sortie standard d'erreur :

```
(defn print-error [message]
  (println "Erreur :" message))

(print-error "Valeur incorrecte")
```

Cela donnera en sortie :

```
Erreur : Valeur incorrecte
```

## Profonde plongée :

L'écriture sur la sortie standard d'erreur est une pratique courante dans la programmation depuis des décennies. Avant l'apparition des systèmes de gestion d'erreurs plus sophistiqués, c'était l'un des seuls moyens pour les programmeurs de signaler des erreurs à l'utilisateur. De nos jours, il existe des alternatives telles que les exceptions, mais écrire sur la sortie standard d'erreur reste une méthode simple et efficace.

## A voir également :

Vous pouvez en apprendre plus sur l'écriture sur la sortie standard d'erreur en consultant la documentation de Clojure : https://clojure.org/reference/exceptions

Si vous cherchez d'autres méthodes de débogage, vous pouvez également consulter cet article sur les outils de débogage en Clojure : https://lambdaisland.com/guides/clojure-debugging-tools