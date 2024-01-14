---
title:    "Ruby: Majuscule d'une chaîne de caractères"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

La capitalisation d'une chaîne de caractères est une tâche courante en programmation, surtout lorsque l'on travaille avec des chaînes de texte provenant de différentes sources. Cela peut également être nécessaire pour respecter certaines conventions d'écriture dans un projet de développement.

## Comment faire

Il existe différents moyens de capitaliser une chaîne de caractères en Ruby, en fonction des besoins et des préférences de chaque développeur. Voici quelques exemples de méthodes couramment utilisées :

```Ruby
# Utilisation de la méthode capitalize
string = "hello world"
puts string.capitalize #=> "Hello world"

# Utilisation de la méthode upcase
string = "hello world"
puts string.upcase #=> "HELLO WORLD"

# Utilisation de la méthode capitalize pour une lettre spécifique
string = "hello world"
puts string.capitalize[0] #=> "H"

# Utilisation de la méthode gsub pour remplacer une lettre par une majuscule
string = "hello world"
puts string.gsub(/\b\w/, &:upcase) #=> "Hello world"
```

Comme on peut le voir dans ces exemples, il est possible de capitaliser une chaîne de caractères en utilisant des méthodes intégrées à Ruby, telles que `capitalize`, `upcase` et `gsub`.

## Plongée en profondeur

Pour comprendre ce qui se passe lorsqu'on capitalise une chaîne de caractères en Ruby, il est important de comprendre comment les caractères sont représentés en arrière-plan. Ruby utilise l'encodage UTF-8 par défaut, ce qui signifie que chaque caractère peut être représenté par un ou plusieurs octets en fonction de sa complexité.

Lorsque l'on utilise des méthodes comme `capitalize` ou `upcase`, Ruby va simplement convertir le premier caractère de la chaîne en majuscule en fonction de sa représentation en arrière-plan. Cela peut sembler simple, mais cela peut poser des problèmes si l'on travaille avec des caractères spéciaux ou des accents.

Afin de s'assurer que la capitalisation est correcte dans toutes les situations, il est important de bien comprendre la façon dont les caractères sont représentés en arrière-plan et de choisir la méthode appropriée en fonction de cela.

## Voir aussi

- [La documentation officielle de Ruby sur les méthodes de chaînes de caractères](https://ruby-doc.org/core-3.0.1/String.html)
- [Un tutoriel sur la capitalisation de chaînes de caractères en Ruby](https://www.rubyguides.com/2017/09/ruby-string-methods/)
- [Une discussion sur Stack Overflow sur les différentes méthodes de capitalisation en Ruby](https://stackoverflow.com/questions/2016536/capitalize-words-in-string-array-using-ruby)