---
title:                "Ruby: Trouver la longueur d'une chaîne de caractères"
programming_language: "Ruby"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

L'une des tâches les plus courantes en programmation est de trouver la longueur d'une chaîne de caractères. Cela peut sembler être une tâche simple, mais il est important de comprendre comment cela fonctionne pour pouvoir utiliser cette fonctionnalité correctement dans vos programmes.

## Comment faire

Pour trouver la longueur d'une chaîne de caractères en Ruby, vous pouvez utiliser la méthode `length` ou `size`. Voici un exemple de code:

```Ruby
phrase = "Bonjour tout le monde!"

puts phrase.length
puts phrase.size
```

Cela va imprimer le nombre de caractères présents dans la chaîne, qui est 21 dans ce cas.

Il est important de noter que les espaces comptent également comme des caractères dans une chaîne. Donc, si vous avez une phrase avec des espaces, cela va également les inclure dans la longueur de la chaîne.

## Plongée en profondeur

Pour trouver la longueur d'une chaîne de caractères, Ruby utilise la propriété `size` de l'objet String. Cette propriété renvoie la valeur de `@length`, qui est une variable d'instance. Cela signifie que chaque fois que vous appelez `size`, il va compter le nombre de caractères dans la chaîne et l'affecter à cette variable.

Ruby utilise également la méthode `length`, qui utilise la propriété `size` en interne pour renvoyer la longueur de la chaîne. Les deux méthodes sont donc équivalentes et la seule différence entre les deux est la préférence personnelle du programmeur.

## Voir aussi

- [Les chaînes de caractères en Ruby](https://www.lucaswillems.com/cours/apprendre-ruby/chaine-de-caracteres/)
- [Méthodes de manipulation de chaînes de caractères en Ruby](https://zetcode.com/lang/rubyr/stringsmethods/)
- [Documentation officielle de Ruby sur les chaînes de caractères](https://ruby-doc.org/core-2.7.2/String.html)