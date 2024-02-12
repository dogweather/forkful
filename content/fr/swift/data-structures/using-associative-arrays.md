---
title:                "Utilisation des tableaux associatifs"
aliases: - /fr/swift/using-associative-arrays.md
date:                  2024-01-30T19:13:08.171943-07:00
model:                 gpt-4-0125-preview
simple_title:         "Utilisation des tableaux associatifs"

tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?

Les tableaux associatifs, connus sous le nom de dictionnaires dans Swift, vous permettent de stocker et de gérer des données sous forme de paires clé-valeur. Les programmeurs les utilisent pour organiser les données de manière efficace, ce qui facilite l'accès et la manipulation des valeurs en fonction de leurs clés uniques.

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
