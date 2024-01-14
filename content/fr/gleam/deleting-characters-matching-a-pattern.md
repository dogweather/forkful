---
title:    "Gleam: Suppression de caractères correspondants à un modèle"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Pourquoi

Supprimer des caractères correspondant à un modèle est une tâche courante en programmation. Cela peut être utile lorsque vous souhaitez supprimer des informations sensibles d'une chaîne de caractères avant de la stocker dans une base de données ou de l'afficher à l'utilisateur.

## Comment faire

Pour supprimer des caractères correspondant à un modèle en utilisant Gleam, vous pouvez utiliser la fonction `String.replace` en spécifiant le modèle à supprimer et une chaîne vide comme valeur de remplacement. Par exemple:

```Gleam
let message = "Bonjour [email protected]"
let clean_message = String.replace(message, "[email protected]", "")
```

Voici l'output attendu:

```
Bonjour 
```

Vous pouvez également utiliser des expressions régulières pour spécifier des modèles plus complexes à supprimer. Par exemple, si vous souhaitez supprimer tous les chiffres d'une chaîne, vous pouvez utiliser l'expression régulière `/\d+/g`. Voici un exemple de code utilisant cette expression régulière pour supprimer tous les chiffres d'une chaîne:

```Gleam
let number = "123Bonjour789"
let clean_number = String.replace(number, /\d+/g, "")
```

L'output sera:

```
Bonjour
```

## Plongée en profondeur

En utilisant des expressions régulières, vous pouvez non seulement supprimer des caractères spécifiques d'une chaîne, mais aussi des motifs de caractères plus complexes. Les expressions régulières peuvent également être utiles lorsque vous voulez supprimer des modèles de caractères récurrents dans une chaîne de données.

Cependant, il est important de noter que l'utilisation d'expressions régulières peut être coûteuse en termes de performances, surtout si elles sont utilisées sur de grandes chaînes de caractères. Il est donc préférable d'utiliser des alternatives plus simples lorsque cela est possible.

## Voir aussi

- La documentation officielle de Gleam sur les chaînes de caractères : https://gleam.run/core/strings
- Un tutoriel sur l'utilisation des expressions régulières en Gleam : https://gleam.run/tutorials/regex
- Un article sur les meilleures pratiques pour traiter les chaînes de caractères en programmation : https://gleam.run/tutorials/strings