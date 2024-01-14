---
title:    "Ruby: Suppression de caractères correspondant à un modèle"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Pourquoi

Vous êtes peut-être en train de travailler sur un projet en Ruby et vous vous demandez pourquoi quelqu'un se donnerait la peine de supprimer des caractères correspondant à un modèle. La réponse est simple: cela peut être utile lors de la manipulation de chaînes de caractères pour s'assurer qu'elles correspondent à un certain format ou pour supprimer des caractères indésirables.

## Comment faire

Pour supprimer des caractères correspondant à un modèle en utilisant Ruby, vous pouvez utiliser la méthode `gsub` qui permet de remplacer un motif par un autre. Voici un exemple de code qui supprime les caractères non numériques d'une chaîne de caractères:

```Ruby
str = "12th May 1992"
new_str = str.gsub(/\D/, "")
puts new_str
```

L'expression régulière `\D` correspond à tous les caractères non numériques, tandis que la chaîne vide `""` indique qu'ils doivent être remplacés par rien. Lorsque vous exécutez ce code, la sortie sera `121992`, où tous les caractères non numériques ont été supprimés.

Vous pouvez également utiliser la méthode `delete` pour supprimer plus facilement une liste spécifique de caractères. Voici un exemple:

```Ruby
str = "Hello World!"
new_str = str.delete("l")
puts new_str
```

La sortie sera `Heo Word!`, où tous les caractères "l" ont été supprimés de la chaîne initiale.

## Plongée en profondeur

Pour une utilisation plus avancée de la suppresion de caractères correspondant à un modèle en utilisant Ruby, vous pouvez également utiliser les expressions régulières pour capturer des groupes de caractères spécifiques à supprimer. Par exemple, si vous souhaitez supprimer tous les caractères compris entre des crochets dans une chaîne, vous pouvez utiliser l'expression régulière `\[.*?\]` pour capturer les caractères entre les crochets et les supprimer.

```Ruby
str = "I have [5] apples and [3] oranges"
new_str = str.gsub(/\[.*?\]/, "")
puts new_str
```

La sortie sera `I have apples and oranges`, où les groupes de caractères entre crochets ont été supprimés.

## Voir aussi

- [Ruby Docs - String#gsub](https://ruby-doc.org/core-2.7.4/String.html#method-i-gsub)
- [Ruby Docs - String#delete](https://ruby-doc.org/core-2.7.4/String.html#method-i-delete)
- [Rubular - Testeur d'expressions régulières en ligne](https://rubular.com/)