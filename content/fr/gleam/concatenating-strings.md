---
title:                "Gleam: Concaténation de chaînes de caractères"
programming_language: "Gleam"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/concatenating-strings.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un programmeur débutant en Gleam, vous pourriez vous demander pourquoi il est important ou utile de concaténer des chaînes de caractères. En d'autres termes, pourquoi vouloir fusionner différentes chaînes en une seule?

La réponse est simple - cela vous permet de créer des chaînes dynamiques et flexibles pour vos programmes. Par exemple, si vous voulez afficher le nom d'un utilisateur avec une salutation, vous pouvez concaténer la chaîne "Bonjour, " avec le nom de l'utilisateur pour créer "Bonjour, John".

Mais comment pouvez-vous le faire en Gleam? Jetons un coup d'œil à la section suivante pour apprendre comment concaténer des chaînes en utilisant Gleam.

## Comment faire

En utilisant la fonction `++` dans Gleam, vous pouvez facilement concaténer deux chaînes ensemble. Jetons un coup d'œil à un exemple:

```Gleam
let nom = "John"
let salutation = "Bonjour, "
let message = salutation ++ nom
```

Ici, nous avons défini deux variables, `nom` et `salutation`, et les avons ensuite concaténées ensemble en utilisant la fonction `++`. Le résultat sera "Bonjour, John".

Vous pouvez également utiliser cette fonction pour concaténer plus de deux chaînes à la fois, en les ajoutant dans l'ordre souhaité. Par exemple:

```Gleam
let nom = "John"
let adjectif = "heureux "
let titre = "présentateur"
let message = nom ++ "est un " ++ adjectif ++ titre
```

Le résultat sera "John est un heureux présentateur".

## Plongée en profondeur

Le processus de concaténation de chaînes est plus complexe qu'il n'y paraît à première vue. En utilisant la fonction `++`, vous créez en fait une toute nouvelle chaîne de caractères, qui est stockée dans la mémoire. Cela peut sembler négligeable avec de petites chaînes, mais cela peut avoir un impact sur les performances avec des chaînes plus longues.

Pour cette raison, il peut être plus efficace de concaténer des chaînes en utilisant une fonction de concaténation spécifique plutôt que d'utiliser la fonction `++`. Cela permet d'économiser de la mémoire et d'améliorer les performances de votre programme. Vous pouvez également envisager d'utiliser des constructions de chaînes de caractères basées sur des modèles, telles que `String.join`, pour améliorer davantage les performances.

## Voir aussi

- [Documentation Gleam sur la concaténation de chaînes](https://gleam.run/articles/strings/)
- [Exemples de code pour la concaténation de chaînes en Gleam](https://github.com/gleam-lang/examples/blob/master/strings_concatenation.gleam)