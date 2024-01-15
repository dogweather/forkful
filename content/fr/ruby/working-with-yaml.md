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

## Pourquoi

Travailler avec YAML peut sembler intimidant pour les nouveaux programmeurs, mais c'est en réalité un format très utile et facile à comprendre pour stocker et échanger des données structurées. Que vous travailliez sur un projet personnel ou professionnel, apprendre à utiliser YAML peut être un ajout précieux à vos compétences en programmation.

## Comment faire

La première étape pour travailler avec YAML est d'installer la gem YAML pour votre projet Ruby. Vous pouvez le faire en utilisant la commande suivante dans votre terminal:

```Ruby
gem install yaml
```

Ensuite, vous devez utiliser la méthode `require` pour charger la gem YAML dans votre code Ruby:

```Ruby
require 'yaml'
```

Maintenant, vous êtes prêt à commencer à travailler avec YAML! Voici un exemple de code pour créer une structure de données simple en YAML et l'écrire dans un fichier:

```Ruby
data = { :nom => 'Jean', :age => 25, :ville => 'Paris' }
yaml_data = data.to_yaml
File.open('fichier.yaml', 'w') { |f| f.write(yaml_data) }
```

Et voici le résultat que vous trouverez dans votre fichier YAML:

```YAML
---
:nom: Jean
:age: 25
:ville: Paris
```

Vous pouvez également utiliser YAML pour lire des données à partir d'un fichier et les stocker dans une variable en utilisant la méthode `load_file`:

```Ruby
mon_fichier = File.open('fichier.yaml', 'r')
donnees = YAML.load_file(mon_fichier)
```

Maintenant, vous pouvez accéder aux données comme vous le feriez avec n'importe quelle autre variable en utilisant les clés que vous avez définies. Par exemple:

```Ruby
puts "Le nom de cette personne est #{donnees[:nom]}"
```

## Plongée en profondeur

YAML est un format de données structuré très flexible et puissant. Il prend en charge plusieurs types de données tels que les chaînes, les nombres, les listes et les structures de données complexes. Vous pouvez également utiliser YAML pour représenter des objets de classe Ruby. Voici un exemple de code qui montre comment le faire:

```Ruby
class Utilisateur
  attr_accessor :nom, :age, :ville
  def initialize(nom, age, ville)
    @nom = nom
    @age = age
    @ville = ville
  end
end

utilisateur = Utilisateur.new('Sophie', 30, 'Lyon')
yaml_data = utilisateur.to_yaml
puts yaml_data
```

Cela produira la sortie suivante:

```YAML
---
!ruby/object:Utilisateur
nom: Sophie
age: 30
ville: Lyon
```

En savoir plus sur YAML et ses fonctionnalités peut être très utile pour créer des applications plus complexes et sophistiquées. N'hésitez pas à explorer la documentation et à expérimenter avec différents types de données et de structures.

## Voir aussi
- [Documentation officielle YAML](https://yaml.org/)
- [Introduction à YAML sur Sitepoint](https://www.sitepoint.com/yaml-tutorial/)
- [Tutoriel YAML sur Codecademy](https://www.codecademy.com/learn/learn-yaml)