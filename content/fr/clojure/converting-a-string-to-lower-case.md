---
title:                "Clojure: Passer une chaîne de caractères en minuscules."
simple_title:         "Passer une chaîne de caractères en minuscules."
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Pourquoi
La conversion d'une chaîne de caractères en lettres minuscules peut être utile lorsque vous travaillez avec des données sensibles à la casse, telles que des mots de passe ou des noms d'utilisateurs.

## Comment faire
```Clojure
; Définition d'une chaîne de caractères en majuscules
(def s "CHERCHEZ-VOUS DE L'AIDE ?")
; Conversion de la chaîne en lettres minuscules
(str/lower-case s)
```
```
=> "cherchez-vous de l'aide ?"
```

## Plongée en profondeur
La fonction `lower-case` utilise l'algorithme Unicode pour convertir chaque caractère en sa version en lettres minuscules. Il est important de noter que cette fonction ne fonctionne qu'avec les caractères ASCII et Unicode, et ne peut pas résoudre les problèmes de casse pour les jeux de caractères plus spécifiques.

## Voir aussi
- [Documentation officielle de la fonction lower-case](https://clojuredocs.org/clojure.string/lower-case)
- [Article sur les différences entre lower-case et capitalize](https://www.tutorialspoint.com/clojure/clojure_string_lower.htm)
- [Exemple pratique de conversion de chaîne en lettres minuscules](https://www.baeldung.com/clojure-string-lower-upper-case)