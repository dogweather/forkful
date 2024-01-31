---
title:                "Travailler avec YAML"
date:                  2024-01-19
html_title:           "Bash: Travailler avec YAML"
simple_title:         "Travailler avec YAML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
YAML, ou "YAML Ain't Markup Language", est un format de serialization facile à lire pour les données. Les devs l'utilisent pour configurer des projets, des containers Docker et dans les pipelines CI/CD car c'est clair et simple à comprendre.

## How to:
Pour travailler avec YAML en Python, la bibliothèque `PyYAML` est souvent utilisée. Voici comment on lit et écrit des données YAML :

```Python
# Installer PyYAML avec pip
# pip install pyyaml

import yaml

# Écrire dans un fichier YAML
data = {'cle': 'valeur', 'liste': [1, 2, 3]}
with open('example.yaml', 'w') as f:
    yaml.dump(data, f)

# Lire un fichier YAML
with open('example.yaml', 'r') as f:
    loaded_data = yaml.safe_load(f)
    print(loaded_data)

```

Sortie :
```
{'cle': 'valeur', 'liste': [1, 2, 3]}
```

## Deep Dive
YAML est apparu pour la première fois en 2001, offrant une alternative humainement lisible au XML et au JSON. Par rapport au JSON, YAML supporte les commentaires et est souvent plus souple avec les structures de données. Attention, cependant, que YAML peut être complexe avec des références et des contenus imbriqués. Des bibliothèques comme `ruamel.yaml` offrent plus de fonctionnalités que `PyYAML`, notamment la préservation des commentaires lors de l'écriture.

## See Also
Pour en savoir plus sur YAML et PyYAML :

- Documentation YAML : https://yaml.org/spec/1.2/spec.html
- PyYAML : https://pyyaml.org/wiki/PyYAMLDocumentation
- ruamel.yaml: https://yaml.readthedocs.io/en/latest/
