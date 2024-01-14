---
title:    "Gleam: Utilisation des expressions régulières"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Pourquoi

Les expressions régulières sont un outil très utile pour trouver, extraire et manipuler des motifs dans du texte. Elles sont particulièrement utiles pour traiter de grandes quantités de données ou pour effectuer des tâches de recherche et de remplacement.

## Comment faire

Pour utiliser des expressions régulières dans le langage de programmation Gleam, il suffit d'importer le module `regex` et de définir l'expression régulière souhaitée à l'aide de la fonction `make()`.

Voici un exemple de code montrant comment trouver et afficher le nombre de fois où un mot spécifique apparaît dans une chaîne de caractères :

```Gleam
import regex

let count = regex.make("mot").find("Voici un exemple de mot et un autre exemple de mot")
count
```

Cela renverra le nombre `2` pour le mot "mot" dans la chaîne de caractères donnée.

## Plongée profonde

Les expressions régulières peuvent sembler compliquées au premier abord, mais elles deviennent très puissantes une fois que vous les maîtrisez. Voici quelques astuces et conseils pour les utiliser efficacement dans vos projets Gleam :

- Utilisez les caractères spéciaux pour trouver des motifs plus spécifiques, tels que les chiffres avec `\d` ou les lettres avec `\w`.
- Utilisez les quantificateurs pour spécifier le nombre de fois qu'un motif doit apparaître, tels que `+` pour une ou plusieurs fois et `*` pour zéro ou plusieurs fois.
- Utilisez les groupes de capture pour extraire des parties spécifiques de votre texte, en les définissant entre parenthèses dans votre expression régulière.
- N'hésitez pas à utiliser des sites tels que [Regex101](https://regex101.com/) pour tester et valider vos expressions régulières avant de les utiliser dans votre code.

En explorant et en pratiquant régulièrement avec des expressions régulières, vous deviendrez rapidement plus à l'aise et pourrez les utiliser facilement pour résoudre des problèmes complexes.

## Voir aussi

- [Documentation officielle de Gleam sur les expressions régulières](https://gleam.run/articles/regex)
- [Guide de référence rapide sur les expressions régulières](https://www.rexegg.com/regex-quickstart.html)
- [Regex101 - Tester et valider vos expressions régulières](https://regex101.com/)

À vous maintenant d'explorer et de découvrir toutes les possibilités qu'offrent les expressions régulières dans vos projets Gleam !