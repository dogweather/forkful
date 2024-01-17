---
title:                "Suppression de caractères correspondant à un motif"
html_title:           "Swift: Suppression de caractères correspondant à un motif"
simple_title:         "Suppression de caractères correspondant à un motif"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faire?
La suppression de caractères correspondant à un motif est une technique de programmation qui consiste à supprimer certaines parties d'une chaîne de caractères en fonction d'un motif prédéfini. Les programmeurs le font généralement pour nettoyer et formater des données, notamment lors de la validation de formulaires ou du traitement de fichiers de texte. Cela peut également aider à améliorer les performances lors de la recherche et du traitement de grandes quantités de données.

## Comment faire:
```Swift
let inputString = "Bonjour tout le monde!"
let filteredString = inputString.filter { $0 != "o" }
print(filteredString) // Bnjur tut le mnde!
```
Dans cet exemple, nous utilisons la fonction `filter` pour supprimer tous les caractères "o" de la chaîne `inputString`. La fonction `filter` prend une closure en paramètre qui spécifie la condition de filtrage, dans ce cas-ci, tous les caractères qui ne sont pas "o" seront conservés. La chaîne filtrée est ensuite imprimée, donnant en résultat "Bnjur tut le mnde!".

## Plongée plus profonde:
La suppression de caractères correspondant à un motif existe depuis longtemps et était initialement utilisée dans les langages de programmation de bas niveau tels que C et Perl. De nos jours, avec l'émergence de langages de haut niveau comme Swift, cette technique s'est popularisée grâce à sa simplicité et son efficacité.

Il existe également d'autres façons de supprimer des caractères correspondant à un motif, telles que l'utilisation d'expressions régulières ou de boucles pour parcourir une chaîne de caractères. Cependant, la méthode `filter` est souvent préférée pour sa lisibilité et sa concision.

Du point de vue de l'implémentation, la fonction `filter` utilise une boucle sous le capot pour parcourir tous les caractères de la chaîne et retourner un nouveau tableau contenant les caractères filtrés. Cela signifie que la complexité de cette opération est linéaire (O(n)), ce qui la rend très efficace pour les grandes entrées de données.

## À voir également:
Pour en savoir plus sur la manipulation de chaînes de caractères en Swift, vous pouvez consulter la documentation officielle sur les `Strings` et les `Characters`. Vous pouvez également découvrir d'autres techniques de manipulation de chaînes telles que la concaténation ou la division de chaînes.