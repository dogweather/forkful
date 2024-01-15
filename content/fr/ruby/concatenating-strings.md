---
title:                "Concaténer des chaînes de caractères"
html_title:           "Ruby: Concaténer des chaînes de caractères"
simple_title:         "Concaténer des chaînes de caractères"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/concatenating-strings.md"
---

{{< edit_this_page >}}

## Pourquoi

Vous vous demandez peut-être pourquoi il est utile de concaténer des chaînes de caractères en utilisant Ruby. Et bien, la concaténation de chaînes de caractères est un moyen simple et efficace de créer une nouvelle chaîne de caractères en combinant plusieurs chaînes existantes.

## Comment procéder

Pour concaténer des chaînes de caractères en Ruby, vous pouvez utiliser l'opérateur `+` ou la méthode `concat()`. Voici un exemple de code pour illustrer cela :

```Ruby
foo = "Hello"
bar = "World!"

result = foo + bar
# Output: "HelloWorld!"

result = foo.concat(bar)
# Output: "HelloWorld!"
```

Comme vous pouvez le voir, les deux méthodes produisent le même résultat. Il est important de noter que la concaténation modifie la chaîne d'origine et crée une nouvelle chaîne dans la mémoire. Par conséquent, il est recommandé d'utiliser l'opérateur `+` lorsque vous concaténez un petit nombre de chaînes, tandis que la méthode `concat()` est plus adaptée pour concaténer un grand nombre de chaînes.

## Plongée en profondeur

Maintenant que vous savez comment concaténer des chaînes de caractères en Ruby, il est temps de comprendre un peu mieux ce qui se passe en coulisses. Lorsque vous utilisez l'opérateur `+`, Ruby crée une nouvelle chaîne en copiant les caractères de chaque chaîne d'origine dans la nouvelle chaîne. Cela peut entraîner une surcharge de mémoire si vous concaténez un grand nombre de chaînes.

En revanche, la méthode `concat()` modifie directement la chaîne d'origine en ajoutant les nouvelles chaînes à la fin. Cela peut être plus efficace en termes de mémoire, mais si vous avez besoin de conserver la chaîne d'origine, vous devrez utiliser l'opérateur `+` ou créer un nouveau stockage pour la chaîne modifiée.

## Voir aussi

- [Documentation officielle Ruby pour la concaténation de chaînes de caractères](https://ruby-doc.org/core-#{RUBY_VERSION}/String.html#method-i-2B)
- [Article sur les différentes façons de concaténer des chaînes en Ruby](https://www.rubyguides.com/2019/08/ruby-string-concatenation/)
- [Guide complet sur les chaînes de caractères en Ruby](https://www.rubyguides.com/2019/08/ruby-strings-explained/)