---
title:                "Ruby: Travailler avec yaml"
simple_title:         "Travailler avec yaml"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/working-with-yaml.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un développeur Ruby ou si vous cherchez à apprendre à programmer en Ruby, vous avez probablement entendu parler du format YAML. Mais pourquoi devriez-vous apprendre à travailler avec YAML ? Eh bien, tout d'abord, YAML (YAML Ain't Markup Language) est un format de données simple et facile à lire, ce qui le rend idéal pour stocker des informations structurées. De plus, il est souvent utilisé comme alternative à XML, ce qui en fait un outil utile pour ceux qui cherchent à simplifier leur code.

## Comment faire

### Installation de la bibliothèque YAML

Avant de pouvoir commencer à utiliser YAML dans votre code Ruby, vous devrez installer la bibliothèque YAML. Vous pouvez le faire en utilisant la commande suivante dans votre terminal :

```ruby
gem install psych
```

### Analyser YAML

Maintenant que vous avez la bibliothèque YAML installée, vous pouvez commencer à analyser des données YAML dans votre code. Voici un exemple de YAML :

```ruby
personne :
  nom : Jean
  age : 30
  profession : développeur
  langages :
  - Ruby
  - JavaScript
```

Pour analyser ces données, vous pouvez utiliser la méthode `YAML.load` et passer en paramètre une chaîne de caractères représentant les données YAML :

```ruby
require 'yaml'

données = YAML.load("
personne :
  nom : Jean
  age : 30
  profession : développeur
  langages :
  - Ruby
  - JavaScript
")
puts données["personne"]["nom"]     # affiche "Jean"
puts données["personne"]["age"]     # affiche 30
puts données["personne"]["langages"] # affiche une liste [Ruby, JavaScript]
```

### Générer YAML

En plus de pouvoir analyser des données YAML, vous pouvez également générer du code YAML à partir de votre code Ruby. Voici un exemple :

```ruby
require 'yaml'

personne = {
  nom: "Marie",
  age: 25,
  profession: "designer",
  langages: ["CSS", "HTML"]
}

puts personne.to_yaml
```

Le code ci-dessus générera un code YAML à partir de l'objet personne et le mettra en forme :

```ruby
nom : Marie
age : 25
profession : designer
langages :
  - CSS
  - HTML
```

## Plongée en profondeur

Bien qu'il existe de nombreuses façons d'utiliser YAML dans votre code Ruby, il est également important de comprendre certains des éléments clés de YAML. Voici quelques points à garder à l'esprit :

- YAML utilise l'indentation pour représenter les hiérarchies de données.
- YAML prend en charge quatre types de données : chaîne, nombre, liste et objet.
- YAML est sensible à la casse, donc faites attention à la façon dont vous écrivez vos clés et valeurs.
- Vous pouvez utiliser des commentaires dans votre code YAML en utilisant le symbole `#`.

## Voir aussi

Maintenant que vous avez une compréhension de base de YAML en Ruby, voici quelques liens utiles pour en apprendre plus :

- [Documentation officielle de la bibliothèque YAML en Ruby](https://ruby-doc.org/stdlib-3.0.0/libdoc/yaml/rdoc/YAML.html)
- [Tutoriel sur YAML en Ruby](https://www.tutorialspoint.com/ruby/working_with_yaml_files.htm)
- [Comparaison entre YAML et JSON](https://dzone.com/articles/yaml-and-json-better-together)