---
title:                "Ruby: Concaténation de chaînes"
simple_title:         "Concaténation de chaînes"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/concatenating-strings.md"
---

{{< edit_this_page >}}

## Pourquoi
Vous êtes peut-être familier avec la programmation de base en Ruby et vous vous demandez pourquoi vous devriez apprendre à concaténer des chaînes de caractères. La concaténation de chaînes de caractères est une compétence importante pour construire des programmes plus robustes et dynamiques. Elle consiste à combiner plusieurs chaînes de caractères pour créer une nouvelle chaîne. Cela peut sembler simple, mais cela peut être extrêmement utile dans des situations où vous devez afficher des informations personnalisées ou dynamiques.

## Comment Faire
La concaténation de chaînes de caractères en Ruby est assez simple grâce à l'utilisation des opérateurs de chaîne. Voici un exemple de code pour concaténer deux chaînes de caractères en utilisant l'opérateur "+" :

```Ruby
first_name = "Marie"
last_name = "Dupont"

full_name = first_name + " " + last_name

puts full_name
```

La sortie de ce code sera "Marie Dupont". Comme vous pouvez le voir, nous avons simplement ajouté des espaces entre les chaînes pour créer une nouvelle chaîne avec le nom complet.

Vous pouvez également utiliser l'opérateur "<<" pour ajouter une chaîne à la fin d'une autre, comme ceci :

```Ruby 
greeting = "Bonjour "

greeting << "Marie"

puts greeting
```

La sortie de ce code sera "Bonjour Marie". L'opérateur "<<" agit comme une méthode de concaténation pour la chaîne précédente sur laquelle il est utilisé.

## Plongée en Profondeur
Il existe d'autres façons de concaténer des chaînes de caractères en Ruby, comme l'utilisation de la méthode "concat" ou l'utilisation de la méthode "sprintf" pour formater les chaînes. Vous pouvez également utiliser des variables d'instance et des méthodes pour concaténer des chaînes de manière plus avancée.

Il est important de noter que la concaténation de chaînes en Ruby peut également être réalisée avec des conteneurs de données tels que des tableaux ou des hashs. Cette technique peut être utile lorsque vous avez besoin de concaténer des données non textuelles, comme des nombres, avec des chaînes de caractères.

## Voir Aussi
Pour en savoir plus sur la concaténation de chaînes de caractères en Ruby, voici quelques liens utiles :

- [Documentation officielle Ruby sur la manipulation de chaînes de caractères](https://ruby-doc.org/core-3.0.1/String.html)
- [Guide de programmation Ruby pour les débutants](https://www.rubyguides.com/ruby-tutorial/string-concatenation/)
- [Vidéo Youtube sur la concaténation de chaînes en Ruby](https://www.youtube.com/watch?v=oP-IKwjyJXg)