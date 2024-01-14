---
title:                "PHP: Travailler avec json"
simple_title:         "Travailler avec json"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/working-with-json.md"
---

{{< edit_this_page >}}

# Pourquoi travailler avec JSON en PHP

JSON (JavaScript Object Notation) est un format de données largement utilisé dans le domaine de la programmation. Il permet de stocker et d'échanger des données de manière efficace et légère. Travailler avec JSON en PHP peut être bénéfique pour de nombreuses raisons, notamment pour la compatibilité avec d'autres langages de programmation et pour l'optimisation des performances.

## Comment faire
JSON est facile à utiliser en PHP grâce à la fonction native "json_encode" qui permet de convertir un tableau en JSON et à la fonction "json_decode" qui permet de convertir du JSON en tableau. Voici un exemple de code pour écrire et lire des données en JSON :

```
<?php
// Tableau à convertir en JSON
$donnees = array(
  'nom' => 'John Doe',
  'age' => 28,
  'ville' => 'Paris'
  );

// Convertir le tableau en JSON
$json = json_encode($donnees);

// Afficher le résultat
echo $json; // {"nom":"John Doe","age":28,"ville":"Paris"}

// Convertir du JSON en tableau
$donnees = json_decode($json, true);

// Afficher le résultat
echo $donnees['nom']; // John Doe
```

## Plongée en profondeur
En travaillant avec JSON en PHP, il est important de comprendre la structure des données JSON et la façon dont elles sont traitées par le langage de programmation. En PHP, les données JSON sont automatiquement converties en tableau associatif, ce qui facilite la manipulation et l'accès aux données. De plus, il est possible de spécifier différentes options lors de la conversion en utilisant la fonction "json_encode", telles que la gestion des caractères spéciaux ou la suppression des valeurs nulles.

## Voir aussi
- Documentation officielle de PHP sur JSON : http://php.net/manual/fr/book.json.php
- Tutoriel sur l'utilisation de JSON en PHP : https://www.php.net/manual/fr/intro.json.php
- Utilisation de l'API REST avec JSON en PHP : https://www.sitepoint.com/use-rest-api-data-application/