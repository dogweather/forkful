---
title:                "Travailler avec YAML"
html_title:           "Bash: Travailler avec YAML"
simple_title:         "Travailler avec YAML"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/working-with-yaml.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

YAML, c'est comme du JSON mais lisible par les humains. Les devs l'utilisent pour des configs, des données ou des fichiers de sauvegarde. C'est simple et ça fait le boulot efficacement.

## Comment faire :

```Ruby
require 'yaml'

# Création d'un hash pour démo
config = {
  'environment' => 'production',
  'api_key' => '12345',
  'enabled' => true
}

# Conversion du hash en YAML
yaml_string = config.to_yaml
puts yaml_string

# Écriture du YAML dans un fichier
File.write('config.yml', yaml_string)

# Lecture d'un fichier YAML
loaded_config = YAML.load_file('config.yml')
puts loaded_config['api_key'] # Affiche 12345
```

## Exploration :

**Contexte historique :**
YAML signifie "YAML Ain't Markup Language" (YAML n'est pas un langage de balisage), un jeu de mots indiquant que c'est pour les données, pas la présentation. Il existe depuis les débuts des années 2000. 

**Alternatives :**
Des formats comme JSON ou XML pourraient être utilisés à la place, mais YAML est plus simple à écrire et à lire pour un humain.

**Détails d'implémentation :**
En Ruby, la librairie Psych, incluse par défaut, s'occupe de parser et d'écrire le YAML. Assurez-vous que votre Ruby est à jour pour avoir la version la plus sécurisée et performante de Psych.

## Voir aussi :

- La spec YAML pour les détails pointus : https://yaml.org/spec/
- Ruby-Doc pour des détails sur Psych lib : https://ruby-doc.org/stdlib-3.0.0/libdoc/psych/rdoc/Psych.html
- Un tutoriel pour une introduction plus douce : https://www.tutorialspoint.com/ruby/ruby_yaml.htm
