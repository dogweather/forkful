---
title:                "Travailler avec json"
html_title:           "Gleam: Travailler avec json"
simple_title:         "Travailler avec json"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/working-with-json.md"
---

{{< edit_this_page >}}

##Pourquoi

Tu te demandes pourquoi tu dois t'aventurer dans le monde du JSON? Eh bien, mon cher, il s'agit d'un format de données largement utilisé dans le développement web et les applications. Donc, si tu veux être un développeur ou une développeuse incontournable, alors tu dois maîtriser JSON.

##Comment faire

Tout d'abord, tu dois savoir que le JSON est un format léger et facile à lire pour les humains, mais il est également simple à utiliser en programmation. Voici comment tu peux travailler avec JSON en utilisant Gleam:

```
// Définir une variable avec du JSON
let json = { "nom": "Marie", "âge": 26, "ville": "Paris" }

// Convertir du JSON en un tableau de clés/valeurs
let tableau = Json.to_pairs(json)
// Maintenant tu peux traiter les données plus facilement

// Ajouter une nouvelle clé/valeur au JSON
let nouveau_json = Json.set(json, "profession", "développeur")
```

Et voilà! C'est tout ce dont tu as besoin pour commencer à travailler avec JSON en utilisant Gleam. Maintenant, passons à une plongée plus profonde dans le sujet.

##Plongée en profondeur

En travaillant avec du JSON en Gleam, il y a quelques points importants à garder à l'esprit.

Tout d'abord, Gleam utilise le type `json::Value` pour représenter les données JSON. Ce type peut prendre plusieurs formes, telles que `Null`, `String`, `Number`, `Array` et `Object`. Tu peux utiliser la fonction `json::decode` pour convertir une chaîne de caractères en `json::Value`.

Deuxièmement, Gleam offre plusieurs fonctions utiles pour travailler avec le JSON, comme `json::get` pour obtenir la valeur d'une clé spécifique, `Json.delete` pour supprimer une clé et sa valeur du JSON, et `json::merge` pour fusionner deux JSON en un seul.

De plus, Gleam possède une validation compile-time pour t'assurer que ton code respecte la structure JSON. Cela signifie que tu éviteras les erreurs à l'exécution et que ton code sera plus robuste.

Et enfin, si tu as affaire à un grand volume de données JSON, Gleam est également capable de les traiter de manière efficace grâce à son multi-processing et sa gestion de la mémoire sans utilisateur.

##Voir aussi

- La documentation officielle de Gleam sur le JSON: https://gleam.run/documentation/stdlib/json
- Un tutoriel sur comment utiliser Gleam et le JSON: https://gleam.run/tutorials/working-with-json
- Un article sur les meilleures pratiques pour travailler avec du JSON en Gleam: https://blog.christopher-ogden.co.uk/posts/working-with-json-in-gleam