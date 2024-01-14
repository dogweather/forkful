---
title:                "Ruby: Extraction de sous-chaînes"
simple_title:         "Extraction de sous-chaînes"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/extracting-substrings.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un développeur en herbe cherchant à améliorer votre maîtrise de Ruby, comprendre comment extraire des sous-chaînes (ou sous-strings en anglais) peut être un excellent moyen d'améliorer vos compétences en programmation. Cela peut sembler être une tâche simple, mais cela peut être extrêmement utile pour manipuler et traiter des données.

## Comment faire

Pour extraire une sous-chaîne en Ruby, vous pouvez utiliser la méthode `slice` ou l'opérateur `[]`. Voici un exemple de code pour vous montrer comment extraire une partie spécifique d'une chaîne :

```Ruby
str = "Bonjour le monde"
puts str.slice(0, 7) # Sortie : Bonjour
puts str[8..-1] # Sortie : le monde
```

La méthode `slice` prend deux arguments : l'indice de départ et la longueur de la sous-chaîne à extraire. L'opérateur `[]` permet également de spécifier une plage d'indices à extraire à l'aide de la notation `..`. Dans cet exemple, `-1` est utilisé pour indiquer que nous voulons extraire jusqu'à la fin de la chaîne.

## Plongée profonde

Maintenant que vous savez comment extraire des sous-chaînes en Ruby, il est important de comprendre que les indices des chaînes commencent à partir de zéro. Ainsi, le premier caractère d'une chaîne aura un indice de `0`.

En plus de cela, il existe également d'autres méthodes utiles pour extraire des sous-chaînes en Ruby, telles que `slice!`, `prepend`, `append`, etc. Prenez le temps de vous familiariser avec ces méthodes pour exploiter pleinement leur potentiel.

## Voir aussi

- [Documentation officielle Ruby sur la méthode slice](https://ruby-doc.org/core-3.0.1/String.html#method-i-slice)
- [Guide sur les méthodes utiles pour manipuler les chaînes en Ruby](https://www.rubyguides.com/2019/08/ruby-string-methods/)
- [Exemples pratiques d'utilisation de slice et d'autres méthodes pour manipuler les chaînes en Ruby](https://www.rubyguides.com/2019/08/slice-vs-slice-in-ruby/)