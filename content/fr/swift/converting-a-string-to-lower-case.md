---
title:                "Convertir une chaîne en minuscules"
html_title:           "PHP: Convertir une chaîne en minuscules"
simple_title:         "Convertir une chaîne en minuscules"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est & Pourquoi ?

La conversion d'une chaîne de caractères en minuscules dans Swift est le processus de transformation de tous les caractères de la chaîne en lettres minuscules. C'est fréquemment réalisé pour faciliter la comparaison et la recherche dans les chaînes de caractères.

## Comment faire :

```Swift
let monChaine = "Bonjour À Tous Les Développeurs Swift"
let chaineEnMinuscules = monChaine.lowercased()

print(chaineEnMinuscules)
/* Sortie :
"bonjour à tous les développeurs swift"
*/
```
Dans cet exemple, `lowercased()` est la méthode qui transforme tous les caractères de 'monChaine' en minuscules.

## Plongée profonde

Historiquement, la conversion de chaînes de caractères en minuscules a été cruciale pour rendre les textes plus accessibles et structurés dans les systèmes informatiques. Dans Swift, `lowercased()` fonctionne en prenant en compte les spécificités de chaque langue à l'aide de la norme Unicode. Par exemple, pour le turc, où "I" est transformé en "ı" et non "i".

Une alternative à `lowercased()` pourrait être d'iterer sur chaque caractère de la chaîne, en vérifiant s'il est en majuscules à l'aide de `isUppercase` puis en le transformant en minuscules avec `lowercased()`. Toutefois, cette alternative manque d'efficacité en termes de performance et de précision linguistique.

La mise en œuvre de la conversion en minuscules est faite en interne par Swift. Elle fonctionne pour toutes les chaînes, y compris celles contenant des emojis ou des symboles spéciaux.

## Voir aussi

- Documentation Apple sur les chaînes et les caractères : https://developer.apple.com/documentation/swift/string
- Unicode : https://home.unicode.org/
- Normes Unicode pour la casse et le pliage : http://www.unicode.org/versions/Unicode13.0.0/ch03.pdf