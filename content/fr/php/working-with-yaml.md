---
title:                "PHP: Travailler avec yaml"
simple_title:         "Travailler avec yaml"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/working-with-yaml.md"
---

{{< edit_this_page >}}

## Pourquoi travailler avec YAML ?

Si vous travaillez avec des données structurées, YAML est un outil incontournable pour faciliter la gestion et la manipulation de ces données. En utilisant YAML, vous pouvez stocker vos données dans un format facilement lisible et modifiable par d'autres développeurs.

## Comment utiliser YAML en PHP

Pour commencer à travailler avec YAML en PHP, vous aurez besoin d'une extension PHP appelée "yaml". Si vous utilisez Ubuntu, vous pouvez l'installer en tapant la commande suivante dans votre terminal :

```PHP
sudo apt-get install php-yaml
```

Ensuite, vous pouvez utiliser la fonction `yaml_parse()` pour convertir une chaîne YAML en tableau PHP. Par exemple :

```PHP
<?php
// Chaîne YAML
$input = "
  student:
    name: Jean
    age: 25
    courses:
      - Mathématiques
      - Physique
      - Chimie
";

// Convertir en tableau PHP
$student_info = yaml_parse($input);

// Afficher le nom de l'étudiant
echo $student_info['student']['name']; // Résultat : Jean
```

Vous pouvez également utiliser la fonction `yaml_emit()` pour créer une chaîne YAML à partir d'un tableau PHP. Par exemple :

```PHP
<?php
// Tableau PHP
$data = ["pays" => "France", "villes" => ["Paris", "Marseille", "Lyon"]];

// Convertir en chaîne YAML
$output = yaml_emit($data);

// Afficher la chaîne YAML
echo $output; 
// Résultat :
// pays: France
// villes:
//   - Paris
//   - Marseille
//   - Lyon
```

## Plongée en profondeur dans YAML

YAML est un langage de sérialisation de données qui permet de représenter des données complexes de manière structurée et lisible par les humains. Il prend en charge les types de données tels que les chaînes, les nombres, les tableaux, les objets, les booléens, etc. De plus, YAML prend en charge les commentaires pour faciliter la compréhension du code.

L'un des avantages les plus intéressants de YAML est qu'il permet de regrouper les données en sections et sous-sections, ce qui facilite la navigation et la manipulation des données.

Il existe également des outils pratiques pour valider et convertir des fichiers YAML en ligne, tels que "YAML Lint" et "YAML to JSON converter".

---
## Voir aussi

- [Documentation officielle de YAML](https://yaml.org/)
- [Extensions PHP pour YAML](https://www.php.net/manual/fr/refs.fileprocess.yaml.php)
- [YAML Lint](https://www.yamllint.com/)
- [YAML to JSON converter](https://www.convertjson.com/yaml-to-json.htm)