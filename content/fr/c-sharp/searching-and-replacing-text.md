---
title:                "C#: Rechercher et remplacer du texte"
simple_title:         "Rechercher et remplacer du texte"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Pourquoi
Lorsque vous travaillez sur un projet de programmation, il est très fréquent d'avoir besoin de rechercher et remplacer du texte dans votre code. Cela peut être pour corriger une erreur, mettre à jour une variable ou tout simplement pour améliorer la lisibilité du code. Dans tous les cas, savoir comment effectuer cette tâche de manière efficace est essentiel pour un développeur.

## Comment faire
En utilisant le langage de programmation C#, il existe plusieurs façons de rechercher et remplacer du texte dans votre code. La méthode la plus simple consiste à utiliser la fonction `Replace()` de la classe `String`. Voici un exemple de code:

```C#
string texte = "Bonjour tout le monde";
string nouveauTexte = texte.Replace("Bonjour", "Hello"); // Le nouveau texte sera "Hello tout le monde"
```

Vous pouvez également utiliser des expressions régulières pour rechercher et remplacer du texte dans des situations plus complexes. Voici un exemple de code utilisant la classe `Regex` :

```C#
string texte = "Il était une fois un chat noir et blanc";
string nouveauTexte = Regex.Replace(texte, "chat", "chien"); // Le nouveau texte sera "Il était une fois un chien noir et blanc"
```

Ces méthodes peuvent également être utilisées dans des boucles pour effectuer des recherches et remplacements sur de nombreux éléments de texte. Il est important de noter que ces fonctions remplacent le texte d'origine et ne créent pas une nouvelle chaîne de caractères.

## Plongée en profondeur
En plus des méthodes ci-dessus, il est possible d'utiliser différentes options lors de la recherche et du remplacement de texte. Par exemple, vous pouvez spécifier si la recherche doit être sensible à la casse ou non, ou encore combien de fois un texte doit être remplacé. Il est également possible d'utiliser des patterns de recherche plus complexes avec les expressions régulières. Pour en savoir plus sur ces options, consultez la documentation officielle sur les fonctions `Replace()` et `Regex.Replace()`.

## Voir aussi
- [Documentation officielle de la méthode Replace](https://docs.microsoft.com/fr-fr/dotnet/api/system.string.replace)
- [Documentation officielle de la méthode Regex.Replace](https://docs.microsoft.com/fr-fr/dotnet/api/system.text.regularexpressions.regex.replace)