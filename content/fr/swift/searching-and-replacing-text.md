---
title:                "Recherche et remplacement de texte"
html_title:           "Arduino: Recherche et remplacement de texte"
simple_title:         "Recherche et remplacement de texte"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?

Rechercher et remplacer un texte est le processus de localisation d'une chaîne spécifique et de son remplacement par une autre. Les programmeurs le font pour gérer efficacement les données textuelles et optimiser le code.

## Comment faire :

Regardez le code Swift ci-dessous qui illustre comment faire :

```Swift
var texte = "J'aime Apple"
texte = texte.replacingOccurrences(of: "Apple", with: "Swift")
print(texte)
// Output: J'aime Swift
```

Dans cet exemple, nous avons cherché la chaîne "Apple" dans la variable `texte` et l'avons remplacée par "Swift" en utilisant la méthode `replacingOccurrences(of:,with:)`.

## Diving Deep

Historiquement, la recherche et le remplacement de texte a été fondamentale pour le traitement de texte, introduite par IBM dans le système DOS/360. L'équivalent Swift moderne offre une mise en œuvre hautement optimisée.

Pour une alternative, vous pouvez utiliser NSRegularExpression pour les motifs complexes. Cependant, la méthode montrée est couramment utilisée pour des opérations simples.

Sous le capot, `replacingOccurrences(of:,with:)` utilise la recherche sur l'algorithme d'appariement de chaînes qui utilise typiquement un algorithme de Boyer-Moore ou Knuth-Morris-Pratt.

## Voir Aussi

1. La documentation officielle d’Apple pour plus d'informations sur la gestion de String : https://developer.apple.com/documentation/swift/string
2. Plus d'exemples de code Swift : https://www.hackingwithswift.com
3. Ressources sur l'histoire de l'informatique : https://www.ibm.com/ibm/history/exhibits/mainframe/mainframe_PP360.html