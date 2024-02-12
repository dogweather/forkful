---
title:                "Travailler avec YAML"
date:                  2024-02-03T19:26:57.926257-07:00
model:                 gpt-4-0125-preview
simple_title:         "Travailler avec YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
YAML, qui signifie "YAML Ain't Markup Language" (YAML n'est pas un langage de balisage), est un format de sérialisation de données lisible par l'homme. Les programmeurs utilisent YAML pour les fichiers de configuration, la messagerie inter-processus et le stockage de données en raison de sa syntaxe simple et de sa facilité de lecture par rapport à d'autres formats comme XML ou JSON.

## Comment faire :
Lire et écrire du YAML en Python implique généralement l'utilisation d'une bibliothèque tierce, `PyYAML` étant la plus populaire. Pour commencer, vous devrez installer PyYAML en exécutant `pip install PyYAML`.

**Exemple : Écrire dans un fichier YAML**

```python
import yaml

data = {'une liste': [1, 42, 3.141, 1337, 'aide', u'€'],
        'une chaîne': 'boo!',
        'un autre dictionnaire': {'foo': 'bar', 'clé': 'valeur', 'la réponse': 42}}

with open('exemple.yaml', 'w') as f:
    yaml.dump(data, f, default_flow_style=False)

# Ceci crée `exemple.yaml` avec les données structurées au format YAML.
```

**Exemple : Lire à partir d'un fichier YAML**

```python
import yaml

with open('exemple.yaml', 'r') as f:
    data_loaded = yaml.safe_load(f)

print(data_loaded)

# Sortie : 
# {'une liste': [1, 42, 3.141, 1337, 'aide', '€'],
#  'une chaîne': 'boo!',
#  'un autre dictionnaire': {'foo': 'bar', 'clé': 'valeur', 'la réponse': 42}}
```

**Utiliser YAML pour la Configuration**

De nombreux programmeurs utilisent YAML pour gérer les configurations d'applications. Voici un exemple de la manière dont on pourrait structurer un fichier de configuration et le lire :

config.yaml :
```yaml
base_de_données:
  hôte: localhost
  port: 5432
  nom_utilisateur: admin
  mot_de_passe: secret
```

Lire le fichier de configuration en Python :
```python
import yaml

with open('config.yaml', 'r') as f:
    config = yaml.safe_load(f)

print(config['base_de_données']['hôte'])  # Sortie : localhost
```

**Gérer les Structures Complexes**

Pour les structures complexes, PyYAML vous permet de définir des objets Python personnalisés. Cependant, assurez-vous d'utiliser des pratiques sûres en utilisant `safe_load` pour éviter d'exécuter des fonctions ou des objets arbitraires.

```python
import yaml

# Définir un objet Python
class Exemple:
    def __init__(self, valeur):
        self.valeur = valeur

# Constructeur personnalisé
def constructeur_exemple(loader, node):
    valeur = loader.construct_scalar(node)
    return Exemple(valeur)

# Ajouter un constructeur pour l'étiquette "!exemple"
yaml.add_constructor('!exemple', constructeur_exemple)

yaml_str = "!exemple 'données'"
loaded = yaml.load(yaml_str, Loader=yaml.FullLoader)

print(loaded.valeur)  # Sortie : données
```

Dans cet extrait, `!exemple` est une étiquette personnalisée utilisée pour instancier un objet `Exemple` avec la valeur 'données' à partir d'une chaîne YAML. Des chargeurs personnalisés comme celui-ci étendent la flexibilité de PyYAML, permettant le traitement de structures de données et de types plus complexes.
