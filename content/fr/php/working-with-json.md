---
title:                "Travailler avec json"
html_title:           "PHP: Travailler avec json"
simple_title:         "Travailler avec json"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/working-with-json.md"
---

{{< edit_this_page >}}

## Pourquoi

JSON est un format de données très populaire, largement utilisé dans la programmation web et les applications mobiles. Il est simple à comprendre et à utiliser, ce qui le rend idéal pour le transfert de données entre différentes plateformes et langages. En plus de cela, la syntaxe JSON est légère et facile à lire pour les humains.

## Comment faire

Pour travailler avec des données JSON en PHP, vous aurez besoin de la fonction native `json_encode()` pour convertir des données PHP en JSON et la fonction `json_decode()` pour convertir des données JSON en objets ou tableaux PHP.

```
// Exemple de conversion de données PHP en JSON
$ville = array(
  'nom' => 'Paris',
  'pays' => 'France',
  'population' => 2148000
);

$jsonville = json_encode($ville);
echo $jsonville;

// Sortie: {"nom":"Paris","pays":"France","population":2148000}

// Exemple de conversion de données JSON en objet PHP
$jsonville = '{"nom":"Paris","pays":"France","population":2148000}';
$objetville = json_decode($jsonville);

echo $objetville->nom; // Output: Paris
echo $objetville->pays; // Output: France
echo $objetville->population; // Output: 2148000
```

## Plongée profonde

Outre les fonctions `json_encode()` et `json_decode()`, PHP offre également d'autres fonctions utiles pour travailler avec JSON. Par exemple, la fonction `json_encode()` prend également en charge des options facultatives pour personnaliser la sortie JSON, telles que `JSON_PRETTY_PRINT` pour un formatage plus propre et plus facile à lire. De plus, PHP a également la possibilité de valider et de filtrer des données JSON en utilisant les fonctions `json_last_error()` et `json_last_error_msg()`.

## Voir aussi

- [Documentation PHP sur JSON](https://www.php.net/manual/fr/book.json.php)
- [Qu'est-ce que JSON et pourquoi est-il si populaire ?](https://www.digitalocean.com/community/tutorials/qu-est-ce-que-json-et-pourquoi-est-ce-si-populaire)