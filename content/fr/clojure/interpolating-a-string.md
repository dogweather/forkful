---
title:                "Interpoler une chaîne de caractères"
html_title:           "Clojure: Interpoler une chaîne de caractères"
simple_title:         "Interpoler une chaîne de caractères"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faire?
Interpoler une chaîne en Clojure signifie insérer des variables ou des expressions dans une chaîne de texte. Cela permet aux programmeurs de créer des chaînes dynamiques qui s'adaptent aux données ou aux conditions en cours d'exécution.

## Comment faire:
Utilisez des accolades {} pour insérer des expressions dans une chaîne de texte. Les expressions seront évaluées et le résultat sera inséré dans la chaîne finale:

```Clojure
(def name "Clojure")
(str "Bienvenue sur " name " !")
```
Le résultat sera: "Bienvenue sur Clojure !"

Vous pouvez également utiliser des symboles pour insérer des variables dans une chaîne:

```Clojure
(def age 10)
(str "Tu as " age " ans.")
```
Le résultat sera: "Tu as 10 ans."

## Plongez plus profondément:
Dans Clojure, il existe plusieurs façons d'interpoler une chaîne de texte, notamment en utilisant la fonction `format` ou en utilisant la syntaxe `~{}`. Il est également possible d'interpoler des chaînes de caractères multilignes en utilisant la syntaxe `~`{}`.

L'interpolation de chaîne en Clojure est inspirée par d'autres langages, tels que Ruby ou Perl, qui utilisent des fonctionnalités similaires pour créer des chaînes dynamiques. Cela permet aux programmeurs de gagner du temps et de produire un code plus lisible et maintenable.

## Voir aussi:
- La documentation officielle de Clojure sur l'interpolation de chaîne: https://clojure.org/guides/strings
- Des exemples pratiques d'interpolation de chaîne en action: https://www.braveclojure.com/core-library
- Le langage de programmation Ruby: https://www.ruby-lang.org/fr/