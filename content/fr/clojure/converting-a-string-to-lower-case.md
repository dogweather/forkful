---
title:                "Conversion d'une chaîne en minuscules"
html_title:           "Clojure: Conversion d'une chaîne en minuscules"
simple_title:         "Conversion d'une chaîne en minuscules"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?
La conversion d'une chaîne de caractères en minuscules est une opération courante dans la programmation qui implique de changer toutes les lettres majuscules en lettres minuscules. Les programmeurs font cela pour rendre les chaînes de caractères cohérentes et plus faciles à manipuler.

## Comment faire:
Voici un exemple de code en Clojure pour convertir une chaîne de caractères en minuscules :
```Clojure
(.toLowerCase "HELLO WORLD")
```
La sortie serait:
```Clojure
"hello world"
```
Vous pouvez également utiliser la fonction `lower-case` pour obtenir le même résultat :
```Clojure
(lower-case "HELLO WORLD")
```
La sortie serait également:
```Clojure
"hello world"
```

## Plongée en profondeur:
Historiquement, la conversion de chaînes de caractères en minuscules a été utilisée pour normaliser les données, c'est-à-dire pour les rendre uniformes et plus facilement traitables. Cependant, avec l'arrivée de Unicode, il existe maintenant des alternatives plus précises pour effectuer cette opération, telles que la fonction `lower-case` que nous avons utilisée dans l'exemple précédent. Pour les programmeurs curieux, il peut également être intéressant de jeter un coup d'œil à l'algorithme de conversion de chaîne en minuscules utilisé par la plate-forme Clojure.

## Voir aussi:
- [Documentation officielle sur la fonction `lower-case` en Clojure](https://clojuredocs.org/clojure.core/lower-case)
- [Article sur l'utilisation d'Unicode pour la conversion de chaînes de caractères en minuscules en Java](https://www.baeldung.com/java-lowercase-string)
- [Guide sur la normalisation de données en informatique](https://searchdatamanagement.techtarget.com/definition/data-normalization)