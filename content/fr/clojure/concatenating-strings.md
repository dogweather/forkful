---
title:                "Concaténation de chaînes"
html_title:           "C: Concaténation de chaînes"
simple_title:         "Concaténation de chaînes"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/concatenating-strings.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?
La concaténation des chaînes fait référence à la liaison de deux ou plusieurs chaînes de caractères en une seule. Les programmeurs font cela pour manipuler et transformer des données textuelles, clefs en programmation.

## Comment faire:
La fonction `str` en Clojure peut être utilisée pour concaténer des chaînes. Voyons certains exemples:

```Clojure
(str "Bonjour, " "comment ça va?")
; Résultat: "Bonjour, comment ça va?"

(str "J'aime " "la " "programmation " "en " "Clojure.")
; Résultat: "J'aime la programmation en Clojure."
```
Note: `str` fonctionne même en combinant des chaînes avec d'autres types:

```Clojure
(str "Le carré de " 5 " est " (* 5 5) ".")
; Résultat: "Le carré de 5 est 25."
```

## Plongée en profondeur
Historiquement, en Clojure, nous avons toujours utilisé `str` pour concaténer des chaînes. C'est simple, propre et efficace.

Une alternative pourrait être d'utiliser `clojure.string/join`, mais il est plus utilisé pour unir des chaînes de caractères avec un délimiteur spécifique. Par exemple:

```Clojure 
(clojure.string/join "-" ["J'aime" "la" "programmation" "en" "Clojure"])
; Résultat: "J'aime-la-programmation-en-Clojure"
```

Quant aux détails de mise en œuvre, `str` transforme chaque argument en une représentation de chaîne avant de les concaténer, rendant le processus très polyvalent.

## Voir aussi
1. Guide officiel de Clojure : https://clojuredocs.org/clojure.core/str
2. Pour des exemples plus détaillés : https://www.learn-clojure.com/concepts/string-concatenation
3. Documentation clojure.string/join : https://clojuredocs.org/clojure.string/join