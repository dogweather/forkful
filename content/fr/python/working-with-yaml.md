---
title:                "Python: Travailler avec yaml"
simple_title:         "Travailler avec yaml"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/working-with-yaml.md"
---

{{< edit_this_page >}}

## Pourquoi travailler avec YAML?

Qu'est-ce que YAML et pourquoi devriez-vous l'utiliser dans vos projets de programmation en Python? YAML (YAML Ain't Markup Language) est un format de sérialisation de données principalement utilisé pour stocker et transférer des données structurées. Il est facile à lire et à écrire pour les humains, tout en étant facilement traité par les ordinateurs. Cela en fait un choix idéal pour stocker des configurations, des données de test et d'autres informations dans vos projets.

## Comment faire?

Pour commencer à travailler avec YAML en Python, vous devez d'abord installer le paquet PyYAML via la commande `pip install PyYAML`.

Pour charger des données à partir d'un fichier YAML, vous pouvez utiliser la fonction `yaml.load()` en spécifiant le chemin du fichier en tant que paramètre. Par exemple:

```Python
import yaml

with open('config.yml', 'r') as file:
    config = yaml.load(file, Loader=yaml.FullLoader)

print(config)
```

Ensuite, vous pouvez accéder aux différentes données dans votre fichier YAML comme un dictionnaire Python. Un dictionnaire peut également être converti en fichier YAML en utilisant la fonction `yaml.dump()`.

Voici un exemple de configuration YAML pour un projet de blog:

```Python
port: 5000
database:
  host: localhost
  user: admin
  password: pa$$w0rd
```

## Plongée en profondeur

Un aspect intéressant de YAML est que vous pouvez inclure des références et des expressions dans votre fichier, ce qui peut rendre vos données plus dynamiques. Par exemple, vous pouvez inclure des variables et les utiliser dans différents endroits dans votre fichier YAML.

De plus, YAML offre également la possibilité d'inclure des commentaires pour rendre votre code plus facilement compréhensible pour les autres développeurs.

Il est important de noter que YAML n'est pas un langage de programmation, mais plutôt une façon de stocker et d'organiser des données. Par conséquent, il ne prend pas en charge les opérations complexes telles que les boucles ou les conditions.

## Voir aussi

Pour en savoir plus sur YAML en Python, vous pouvez consulter les ressources suivantes:

- [Documentation officielle de PyYAML](https://pyyaml.org/wiki/PyYAMLDocumentation)
- [Guide de YAML pour les débutants](https://www.cloudbees.com/blog/yaml-tutorial-everything-you-need-get-started/)
- [Python YAML: comment l'utiliser comme un pro](https://code-maven.com/python-yaml-how-to-read-and-write-yaml-files)