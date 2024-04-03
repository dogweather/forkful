---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:20.538766-07:00
description: "Comment faire : Ruby est livr\xE9 avec une biblioth\xE8que int\xE9gr\xE9\
  e appel\xE9e Psych pour l'analyse et l'\xE9mission de YAML. Pour l'utiliser, vous\
  \ devez d'abord\u2026"
lastmod: '2024-03-13T22:44:58.442351-06:00'
model: gpt-4-0125-preview
summary: "Ruby est livr\xE9 avec une biblioth\xE8que int\xE9gr\xE9e appel\xE9e Psych\
  \ pour l'analyse et l'\xE9mission de YAML."
title: Travailler avec YAML
weight: 41
---

## Comment faire :
Ruby est livré avec une bibliothèque intégrée appelée Psych pour l'analyse et l'émission de YAML. Pour l'utiliser, vous devez d'abord requérir la bibliothèque standard YAML. Voici un exemple de base pour commencer :

```ruby
require 'yaml'

# Hash à sérialiser
person = { name: "John Doe", age: 30, skills: ["Ruby", "JavaScript"] }

# Conversion du hash en YAML
yaml_data = person.to_yaml

puts yaml_data
```

**Sortie d'exemple :**

```yaml
---
:name: John Doe
:age: 30
:skills:
- Ruby
- JavaScript
```

Pour charger des données YAML dans un objet Ruby :

```ruby
loaded_person = YAML.load(yaml_data)

puts loaded_person
```

**Sortie d'exemple :**

```ruby
{name: "John Doe", age: 30, skills: ["Ruby", "JavaScript"]}
```

### Utilisation de bibliothèques tierces :
Bien que la bibliothèque standard soit suffisante pour les tâches de base, pour des besoins complexes, vous pouvez envisager des gemmes tierces comme 'safe_yaml'. Pour utiliser de telles bibliothèques, vous devez d'abord installer la gemme :

```bash
gem install safe_yaml
```

Ensuite, vous pouvez l'utiliser pour charger en toute sécurité des données YAML, atténuant les risques comme l'instanciation d'objets à partir de sources contrôlées par l'utilisateur :

```ruby
require 'safe_yaml'

safe_loaded_person = SafeYAML.load(yaml_data)

puts safe_loaded_person
```

**Sortie d'exemple :**

```ruby
{name: "John Doe", age: 30, skills: ["Ruby", "JavaScript"]}
```

Cette approche améliore la sécurité de votre gestion de YAML, en faisant un bon choix pour les applications qui chargent du YAML à partir de sources non fiables.
