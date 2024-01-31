---
title:                "Utilisation des tableaux associatifs"
date:                  2024-01-30T19:13:16.607957-07:00
model:                 gpt-4-0125-preview
simple_title:         "Utilisation des tableaux associatifs"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Les tableaux associatifs, ou objets en TypeScript, vous permettent d'utiliser des chaînes de caractères (ou clés) pour accéder aux paires de valeurs. Les programmeurs les utilisent pour des motifs d'accès aux données plus dynamiques par rapport aux tableaux traditionnels, offrant une manière flexible de structurer et d'accéder aux données sans être lié à des indices numériques.

## Comment faire :

Créer et utiliser des tableaux associatifs en TypeScript est simple. Voici un exemple de base :

```TypeScript
// Déclarer un tableau associatif
let utilisateur: { [cle: string]: string } = {};

// Ajouter des données
utilisateur["nom"] = "Jane Doe";
utilisateur["email"] = "jane@example.com";

console.log(utilisateur);
```

Sortie :

```TypeScript
{ nom: 'Jane Doe', email: 'jane@example.com' }
```

Itérer sur les paires clé-valeur est également facile :

```TypeScript
for (let cle in utilisateur) {
    console.log(cle + ": " + utilisateur[cle]);
}
```

Sortie :

```TypeScript
nom: Jane Doe
email: jane@example.com
```

Et si vous gérez un mélange de types de données, le système de types de TypeScript est très pratique :

```TypeScript
let typesMixtes: { [cle: string]: string | nombre } = {};
typesMixtes["nom"] = "John Doe";
typesMixtes["age"] = 30;

console.log(typesMixtes);
```

Sortie :

```TypeScript
{ nom: 'John Doe', age: 30 }
```

## Approfondissement

En TypeScript, ce que nous appelons des tableaux associatifs sont essentiellement des objets. Historiquement, dans des langues comme PHP, les tableaux associatifs sont un type fondamental, mais JavaScript (et par extension, TypeScript) utilise des objets à cette fin. Cette approche est à la fois une force et une limitation. Les objets fournissent une structure hautement dynamique pour associer des chaînes aux valeurs, mais ils ne sont pas destinés à être utilisés comme des 'tableaux' dans le sens traditionnel. Par exemple, vous ne pouvez pas utiliser directement sur ces objets des méthodes de tableau comme `push` ou `pop`.

Pour les cas où vous avez besoin de collections ordonnées de paires clé-valeur avec des opérations semblables à celles des tableaux, TypeScript (et le JavaScript moderne) offre l'objet `Map` :

```TypeScript
let carteUtilisateur = new Map<string, string>();
carteUtilisateur.set("nom", "Jane Doe");
carteUtilisateur.set("email", "jane@example.com");

carteUtilisateur.forEach((valeur, cle) => {
    console.log(cle + ": " + valeur);
});
```

Alors que le système de types de TypeScript et les fonctionnalités ES6 comme `Map` offrent des alternatives puissantes, comprendre comment utiliser les objets comme tableaux associatifs est utile pour les scénarios où les littéraux d'objets sont plus efficaces ou lors du travail avec des structures de données JSON. Il s'agit de choisir le bon outil pour la tâche.
