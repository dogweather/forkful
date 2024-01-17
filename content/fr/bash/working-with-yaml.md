---
title:                "Travailler avec YAML."
html_title:           "Bash: Travailler avec YAML."
simple_title:         "Travailler avec YAML."
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/working-with-yaml.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le font-ils? 
YAML est un format de données structuré utilisé principalement dans le développement logiciel pour stocker et échanger des configurations. Les programmeurs utilisent YAML car il est facile à lire et à comprendre pour les humains.

## Comment faire?
Pour travailler avec YAML, vous pouvez utiliser des outils tels que "yq" ou "jq" pour traiter et modifier les données dans un fichier YAML. Par exemple, pour afficher le contenu d'un fichier YAML, vous pouvez utiliser la commande suivante dans votre terminal Bash: 
```Bash 
cat mon_fichier.yaml 
```

Pour modifier le contenu d'un fichier YAML, vous pouvez utiliser l'outil "yq" avec l'option "-i" pour modifier directement le fichier d'origine. Par exemple, pour changer la valeur de "name" à "John" dans un fichier YAML, vous pouvez utiliser la commande suivante:
```Bash
yq -i '.name="John"' mon_fichier.yaml
```

## Plongée en profondeur
Le format YAML a été créé en 2001 pour fournir une alternative plus simple et plus lisible au format XML. Il est basé sur la syntaxe du langage de programmation Python et utilise l'indentation plutôt que les balises pour délimiter les données. D'autres formats de données populaires utilisés dans le développement sont JSON et XML, mais YAML est préféré pour sa simplicité et sa lisibilité.

## Voir aussi
Pour en savoir plus sur YAML et son utilisation dans le développement logiciel, vous pouvez consulter la documentation officielle sur https://yaml.org/. Vous pouvez également consulter les alternatives telles que JSON et XML pour voir les différences et les cas d'utilisation appropriés.