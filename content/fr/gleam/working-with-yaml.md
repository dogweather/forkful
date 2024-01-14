---
title:                "Gleam: Travailler avec yaml"
simple_title:         "Travailler avec yaml"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/working-with-yaml.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous travaillez avec des données structurées, il est probable que vous ayez déjà entendu parler de YAML. Ce langage de sérialisation de données est de plus en plus populaire grâce à sa simplicité et sa lisibilité. Si vous souhaitez automatiser ou simplifier la gestion de votre configuration, travailler avec YAML peut être très utile.

## Comment faire

Pour commencer à travailler avec YAML, vous devez d'abord comprendre sa structure de base. YAML utilise une syntaxe basée sur l'indentation avec des niveaux, ce qui facilite la lecture du code. Voici un exemple de code Gleam utilisant YAML :

```Gleam
import gleam/yaml

// Création d'une carte pour stocker les données
data = yaml.new_map()

// Ajouter des clés et valeurs à la carte
yaml.put(data, "nom", "Jean")
yaml.put(data, "âge", 30)

// Convertir la carte en YAML
resultat = yaml.to_string(data)

// Afficher le résultat
Debug.format("Les données sont : {}", [resultat])
```

Le résultat de ce code sera :

```
Les données sont : |- 
  nom: Jean
  âge: 30
```

Comme vous pouvez le constater, YAML est facile à écrire et à lire. Vous pouvez également utiliser des tableaux pour stocker des données. Voici un exemple :

```Gleam
import gleam/yaml

// Création d'un tableau pour stocker les données
data = yaml.new_array()

// Ajouter des valeurs au tableau
yaml.append(data, "rouge")
yaml.append(data, "vert")
yaml.append(data, "bleu")

// Convertir le tableau en YAML
resultat = yaml.to_string(data)

// Afficher le résultat
Debug.format("Les données sont : {}", [resultat])
```

Le résultat sera :

```
Les données sont : [rouge, vert, bleu]
```

Vous pouvez également utiliser Gleam pour lire des données à partir d'un fichier YAML, modifier les données existantes et les convertir en d'autres formats si nécessaire.

## Plongée en profondeur

Si vous souhaitez en savoir plus sur YAML et comment travailler avec ce langage de sérialisation de données en profondeur, vous pouvez consulter la documentation de Gleam sur YAML et les ressources suivantes :

- Site officiel de YAML : https://yaml.org/
- Documentation complète de Gleam : https://gleam.run/documentation/
- Bibliothèque Gleam pour travailler avec YAML : https://github.com/lpil/gleam-yaml

## Voir aussi

- [Introduction to YAML for Beginners](https://www.educative.io/blog/yaml-tutorial-for-beginners)
- [Using YAML to organize your configuration files](https://medium.com/@thinkrefactoring/gentle-introduction-to-yaml-13a11a8dc99d)
- [Mastering YAML: Syntax, engines, and pitfalls](https://rollout.io/blog/mastering-yaml/)