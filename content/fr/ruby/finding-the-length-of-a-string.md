---
title:                "Trouver la longueur d'une chaîne de caractères"
html_title:           "Ruby: Trouver la longueur d'une chaîne de caractères"
simple_title:         "Trouver la longueur d'une chaîne de caractères"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

La longueur d'une chaîne de caractères, également appelée longueur de la chaîne, est le nombre total de caractères dans cette chaîne. Les programmeurs utilisent souvent des fonctions pour trouver la longueur d'une chaîne afin de manipuler ou de traiter des données de manière efficace.

## Comment faire :

Voici un exemple de code Ruby pour trouver la longueur d'une chaîne de caractères :

```Ruby
string = "Bonjour le monde"
puts string.length
```

La sortie devrait être :

```
15
```

Si vous souhaitez compter le nombre de caractères spécifiques dans une chaîne, vous pouvez utiliser la méthode `count` :

```Ruby
string = "foo bar baz"
puts string.count("a")
```

La sortie devrait être :

```
2
```

## Plongée Profonde :

La fonction de la longueur de la chaîne existe depuis les premiers jours de la programmation informatique. Elle est disponible dans de nombreux langages de programmation tels que C, Java et Python. En Ruby, la méthode `length` est une fonction intégrée dans la classe String.

Il existe également d'autres méthodes pour trouver la longueur d'une chaîne, telles que `bytesize` qui renvoie le nombre de bytes utilisés pour stocker la chaîne. Vous pouvez également utiliser la méthode `bytes`, qui retourne un tableau contenant tous les bytes de la chaîne.

## Voir aussi :

Pour en savoir plus sur les différentes fonctions disponibles pour manipuler les chaînes de caractères en Ruby, vous pouvez consultez la documentation officielle : [https://ruby-doc.org/core-3.0.2/String.html](https://ruby-doc.org/core-3.0.2/String.html)