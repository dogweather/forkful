---
title:                "Travailler avec yaml"
html_title:           "Ruby: Travailler avec yaml"
simple_title:         "Travailler avec yaml"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/working-with-yaml.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est & Pourquoi?

Travailler avec YAML signifie écrire du code pour lire et écrire des données au format YAML. Les programmeurs utilisent YAML car c'est un format de données simple et lisible par les humains, ce qui le rend pratique pour stocker et échanger des informations.

## Comment faire:

Voici un exemple de code qui utilise la bibliothèque YAML pour lire un fichier et afficher son contenu:

```Ruby
require 'yaml'

data = YAML.load(File.read('mon_fichier.yml')) # charger les données à partir du fichier YAML
puts data.inspect # affiche les données lues
```

Et voici un exemple de code qui utilise la bibliothèque YAML pour écrire des données dans un fichier:

```Ruby
require 'yaml'

data = { :cle_1 => 'valeur 1', :cle_2 => 'valeur 2' } # créer un hash contenant les données à écrire
File.write('mon_fichier.yml', data.to_yaml) # écrire les données au format YAML dans le fichier
```

## Plongée en profondeur:

YAML (YAML Ain't Markup Language) a été créé en 2001 pour fournir une alternative plus simple et plus facile à utiliser à l'XML. Il est largement utilisé dans les applications web et les projets open source pour la configuration et le stockage de données. Bien qu'il soit principalement utilisé en tant que format de données, YAML peut également être utilisé en tant que langage de balisage.

Une alternative populaire à YAML est le format JSON, qui est plus couramment utilisé pour les applications web et les échanges de données. Cependant, YAML offre des fonctionnalités supplémentaires telles que le support des références et des types de données personnalisés.

Pour implémenter YAML dans votre code Ruby, vous pouvez utiliser la bibliothèque officielle YAML ou des alternatives telles que Psych ou Yamlrb.

## Voir aussi:

- Documentation officielle YAML: https://yaml.org/spec/
- Bibliothèque YAML Ruby: https://ruby-doc.org/stdlib-2.6.3/libdoc/yaml/rdoc/YAML.html
- Yamlrb: https://github.com/apluslms/yamlrb