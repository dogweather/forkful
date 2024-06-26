---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:13:08.171943-07:00
description: "Comment faire : Swift rend le travail avec les tableaux associatifs\
  \ simple. Voici comment vous pouvez d\xE9clarer, ajouter, supprimer et acc\xE9der\
  \ aux \xE9l\xE9ments\u2026"
lastmod: '2024-03-13T22:44:58.210633-06:00'
model: gpt-4-0125-preview
summary: Swift rend le travail avec les tableaux associatifs simple.
title: Utilisation des tableaux associatifs
weight: 15
---

## Comment faire :
Swift rend le travail avec les tableaux associatifs simple. Voici comment vous pouvez déclarer, ajouter, supprimer et accéder aux éléments dans un dictionnaire Swift :

```Swift
// Déclarer un dictionnaire
var fruitColors: [String: String] = ["Apple": "Red", "Banana": "Yellow"]

// Ajouter un nouvel élément
fruitColors["Grape"] = "Purple"

// Accéder à une valeur en utilisant sa clé
if let appleColor = fruitColors["Apple"] {
    print("Apple is \(appleColor).")  // Output: Apple is Red.
} else {
    print("Couleur non trouvée.")
}

// Supprimer un élément
fruitColors["Banana"] = nil  // Cela supprimera "Banana" du dictionnaire

// Itérer sur les éléments
for (fruit, color) in fruitColors {
    print("\(fruit) is \(color).")
    // Sortie :
    // Apple is Red.
    // Grape is Purple.
}
```

Les dictionnaires sont incroyablement polyvalents, permettant de manipuler et d'accéder aux données de manière dynamique. Leur nature non ordonnée n'affecte pas la vitesse de récupération des données, ce qui est un avantage significatif lors du traitement de grands ensembles de données.

## Approfondissement
L'implémentation des dictionnaires en tant que tableau associatif par Swift découle de leur puissante capacité à mapper des clés uniques à des valeurs. Historiquement, les langages de programmation ont mis en œuvre ce concept sous divers noms tels que les tables de hachage ou les cartes, faisant allusion à leur fonctionnalité de créer une "carte" entre les clés et les valeurs.

Dans Swift, les dictionnaires sont optimisés pour la performance, tirant parti des clés hachables pour une récupération efficace des données. Cela signifie que le type `Key` dans un dictionnaire `[Key: Value]` doit se conformer au protocole `Hashable`, ce qui est le cas pour la plupart des types standard Swift comme `Int`, `String` et `Double`.

Une chose à considérer est que, bien que les dictionnaires soient excellents pour associer des paires de données, ils manquent d'ordre. Si vous avez besoin de maintenir l'ordre des éléments, vous pourriez explorer des alternatives comme `Array` pour une séquence d'éléments ordonnés ou des structures de données personnalisées qui combinent les caractéristiques des tableaux et des dictionnaires.

Il est également à noter que Swift évolue continuellement, de même que sa gestion et ses optimisations des dictionnaires. Par conséquent, rester à jour avec la dernière documentation Swift est crucial pour tirer le meilleur parti des dictionnaires, en s'assurant que vous utilisez les pratiques les plus efficaces et à jour.
