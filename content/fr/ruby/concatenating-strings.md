---
title:    "Ruby: Concaténation de chaînes de caractères"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/concatenating-strings.md"
---

{{< edit_this_page >}}

## Pourquoi

La concaténation de chaînes de caractères est une technique couramment utilisée en programmation pour combiner plusieurs morceaux de texte en une seule chaîne. Elle est particulièrement utile dans les situations où vous devez construire dynamiquement un message en combinant des variables ou en ajoutant des mots-clés prédéfinis à des phrases. Dans cet article, nous allons vous montrer comment utiliser la concaténation de chaînes en Ruby pour améliorer votre code et rendre vos messages plus dynamiques.

## Comment Faire

En Ruby, la concaténation de chaînes est réalisée en utilisant l'opérateur `+`. Voici un exemple :

```Ruby
first_name = "Jean"
last_name = "Dupont"
full_name = first_name + " " + last_name

puts full_name
```

Ce bloc de code crée trois variables : `first_name` pour le prénom, `last_name` pour le nom de famille et `full_name` pour le nom complet. L'opérateur `+` est utilisé pour concaténer les variables avec un espace pour former `Jean Dupont` et le résultat est imprimé à l'écran.

La concaténation n'est pas limitée aux seules variables. Vous pouvez également utiliser des chaînes de caractères littérales et même des méthodes pour manipuler les valeurs. Voici un autre exemple :

```Ruby
message = "Aujourd'hui, nous célébrons "
occasion = "l'anniversaire de mariage "
years = 10
full_message = message + occasion + "de nos " + years.to_s + " ans!"

puts full_message
```

Ce code produit l'output suivant : `Aujourd'hui, nous célébrons l'anniversaire de mariage de nos 10 ans!`.

## Plongée Profonde

Maintenant que nous savons comment utiliser l'opérateur `+` pour concaténer des chaînes de caractères, il est important de noter que ce n'est pas la seule façon de le faire en Ruby. Il existe également une méthode de `String` appelée `concat` que vous pouvez utiliser pour ajouter une chaîne à une autre. Voici un exemple :

```Ruby
first_name = "Marie"
last_name = "Lambert"
full_name = first_name.concat(" ", last_name)

puts full_name
```

Ce bloc de code produit le même résultat que notre premier exemple. La différence est que cette méthode modifie la chaîne originale, tandis que l'opérateur `+` crée une nouvelle chaîne.

Un autre aspect important de la concaténation en Ruby est que vous pouvez utiliser des techniques plus avancées pour la rendre plus efficace et élégante. Par exemple, vous pouvez utiliser la méthode `interpolate` pour inclure des variables dans une chaîne sans avoir à concaténer manuellement. Voici un exemple :

```Ruby
first_name = "Nicolas"
last_name = "Perrin"
full_name = "#{first_name} #{last_name}"

puts full_name
```

Ce code produit toujours le même résultat, mais il est plus facile à lire et à comprendre.

## Voir Aussi

Pour en savoir plus sur la concaténation de chaînes en Ruby, consultez les ressources suivantes :

- [Documentation officielle de Ruby](https://ruby-doc.org/core-2.7.0/String.html#method-i-2B)
- [Article sur la manipulation de chaînes de caractères en Ruby](https://www.rubyguides.com/2019/01/ruby-string-manipulation/)
- [Tutoriel de la concaténation en Ruby](https://www.pragmaticlinux.com/2019/08/how-to-concatenate-strings-in-ruby/)

Maintenant que vous avez appris à concaténer des chaînes de caractères en Ruby, n'hésitez pas à l'utiliser dans vos projets pour rendre votre code plus dynamique et facile à maintenir !