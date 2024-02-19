---
aliases:
- /fr/swift/removing-quotes-from-a-string/
date: 2024-01-26 03:41:48.698710-07:00
description: "Supprimer les guillemets d'une cha\xEEne signifie retirer toutes les\
  \ marques de citation qui encadrent le contenu. Nous faisons cela pour assainir\
  \ les\u2026"
lastmod: 2024-02-18 23:09:09.195553
model: gpt-4-0125-preview
summary: "Supprimer les guillemets d'une cha\xEEne signifie retirer toutes les marques\
  \ de citation qui encadrent le contenu. Nous faisons cela pour assainir les\u2026"
title: "Retirer les guillemets d'une cha\xEEne"
---

{{< edit_this_page >}}

## Quoi et pourquoi?

Supprimer les guillemets d'une chaîne signifie retirer toutes les marques de citation qui encadrent le contenu. Nous faisons cela pour assainir les entrées, préparer les données pour le stockage, ou éliminer la mise en forme de texte inutile qui pourrait interférer avec le traitement des données.

## Comment faire :

Swift vous permet de vous attaquer à la tâche de suppression des guillemets assez habilement. Voici un rapide exemple en utilisant `replacingOccurrences(of:with:)`, qui fait exactement ce que cela semble indiquer—remplacer des morceaux de texte par quelque chose d'autre, ou par rien du tout.

```swift
var quotedString = "\"Ceci est une chaîne 'citée'.\""
let unquotedString = quotedString.replacingOccurrences(of: "\"", with: "")
print(unquotedString) // Ceci est une chaîne 'citée'.

// Vous traitez les guillemets simples ? Changez simplement le terme de recherche.
quotedString = "'Voici un autre exemple.'"
let singleQuoteRemoved = quotedString.replacingOccurrences(of: "'", with: "")
print(singleQuoteRemoved) // Voici un autre exemple.
```

La sortie sera des chaînes sans guillemets, toutes prêtes pour ce que vous avez prévu ensuite.

## Plongée Profonde

Nous "nettoyons" des chaînes comme celles-ci depuis l'aube de la programmation. Au début, il s'agissait davantage de conserver la précieuse mémoire et d'éviter les erreurs de syntaxe lors du traitement des entrées. Aujourd'hui, il s'agit de bonne hygiène des données—surtout lorsqu'il s'agit de traiter avec JSON ou de préparer les chaînes pour les travaux en base de données. Un guillemet égaré peut jeter un pavé dans la mare des requêtes SQL plus vite que vous ne pourriez dire "erreur de syntaxe."

Des alternatives ? Eh bien, si vous trouvez `replacingOccurrences(of:with:)` un peu trop simple, vous pourriez vous plonger dans les expressions régulières pour des motifs plus complexes ou lorsque vous souhaitez supprimer les guillemets uniquement à certaines positions. La classe `NSRegularExpression` de Swift est votre amie ici. Mais rappelez-vous, les regex peuvent être une épée à double tranchant—puissantes, mais parfois excessives.

En terme d'implémentation, `replacingOccurrences(of:with:)` est une méthode fournie par `String` dans Swift, qui appelle en interne des fonctions de manipulation de chaînes plus complexes qui gèrent l'Unicode et d'autres subtilités du traitement moderne du texte. C'est l'un de ces arrangements "simples en surface, complexes sous le capot" que Swift gère pour vous afin que vous n'ayez pas à le faire.

## Voir Aussi

Pour en savoir plus sur les manipulations de chaînes en Swift :

- Le Langage de Programmation Swift (Chaînes et Caractères) : [Documentation Swift.org](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- NSRegularExpression : [Documentation pour les développeurs d'Apple](https://developer.apple.com/documentation/foundation/nsregularexpression)

Et si vous êtes maintenant curieux à propos des expressions régulières et souhaitez tester vos motifs :

- Regex101 : [Testeur et Débogueur Regex](https://regex101.com)
