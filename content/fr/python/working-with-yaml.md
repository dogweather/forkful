---
title:                "Travailler avec yaml"
html_title:           "Python: Travailler avec yaml"
simple_title:         "Travailler avec yaml"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/working-with-yaml.md"
---

{{< edit_this_page >}}

## Pourquoi travailler avec YAML?

Si vous travaillez avec des données structurées ou des configurations, YAML est un langage de balisage qui peut vous aider à simplifier votre travail. Il est lisible pour les humains et facile à utiliser pour organiser des données complexes.

## Comment faire

Pour utiliser YAML dans votre code Python, vous devez d'abord installer le module PyYAML en utilisant pip ou votre outil de gestion de packages préféré. Ensuite, vous pouvez importer le module dans votre script Python en utilisant la commande `import yaml`. Vous pouvez ensuite commencer à créer et à manipuler des données YAML en utilisant le formatage suivant :

```Python
import yaml

# Créer un dictionnaire avec des données
donnees = {
    'nom': 'Jean',
    'âge': 30,
    'métier': 'développeur'
}

# Convertir les données en YAML
donnees_yaml = yaml.dump(donnees)

# Imprimer les données YAML
print(donnees_yaml)

# Sortie :
# nom: Jean
# âge: 30
# métier: développeur
```

Vous pouvez également utiliser YAML pour charger des données YAML dans votre code Python et les manipuler en tant qu'objets Python :

```Python
import yaml

# Charger les données YAML à partir d'un fichier
with open('donnees.yaml') as f:
    donnees = yaml.load(f, Loader=yaml.FullLoader)

# Imprimer la valeur de la clé 'âge'
print(donnees['âge'])

# Sortie :
# 30
```

## Plongée en profondeur

YAML est un langage de balisage très flexible et peut être utilisé pour créer des structures de données complexes telles que des listes, des tuples et des dictionnaires imbriqués. Il est également possible d'inclure des commentaires dans un fichier YAML en utilisant le symbole `#`.

Il est important de noter que YAML utilise une indentation pour marquer les niveaux de hiérarchie des données. Vous devez donc être attentif aux espaces ou aux tabulations utilisés dans votre formatage YAML.

Enfin, il existe plusieurs options de configuration pour le module PyYAML, notamment pour gérer des cas spéciaux tels que les données binaires ou les dates. Vous pouvez en savoir plus sur ces options dans la documentation officielle de PyYAML.

## Voir aussi

- Documentation officielle de PyYAML : https://pyyaml.org/
- Tutoriel de YAML pour les débutants : https://www.youtube.com/watch?v=cdLNKUoMc6c
- Utilisation de YAML avec Django : https://simpleisbetterthancomplex.com/tutorial/2016/10/13/how-to-use-yaml-serializer.html