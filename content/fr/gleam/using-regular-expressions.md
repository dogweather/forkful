---
title:                "Gleam: Utiliser les expressions régulières"
programming_language: "Gleam"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Pourquoi

Les expressions régulières sont un outil puissant pour rechercher des motifs spécifiques dans du texte. Avec Gleam, vous pouvez facilement utiliser des expressions régulières dans votre code pour filtrer et manipuler des données de manière efficace. Que vous souhaitiez traiter de grandes quantités de données ou simplement trouver un mot spécifique dans une chaîne de caractères, les expressions régulières sont un outil précieux pour tout programmeur.

## Comment faire

Pour utiliser des expressions régulières en Gleam, vous devez d'abord importer le module `regex`. Ensuite, vous pouvez créer un nouveau pattern en utilisant la fonction `Regex.make()` et en passant le motif en tant que chaîne de caractères entre parenthèses. Par exemple, si vous souhaitez trouver tous les mots contenant "chat" dans une liste, vous pouvez utiliser le motif `"(chat)"`. Ensuite, vous pouvez utiliser la fonction `Regex.matches()` pour vérifier si une chaîne de caractères correspond à ce motif spécifique. Voici un exemple de code:

```Gleam
import regex

animals = ["chien", "chat", "souris", "oiseau"]
pattern = Regex.make("(chat)")
matches = Regex.matches(pattern, "J'ai un chat")
// Output: matches = [Some("chat")]
```

Vous pouvez également utiliser des expressions régulières pour remplacer des parties spécifiques d'une chaîne de caractères. Dans l'exemple suivant, nous allons remplacer toutes les occurrences de "a" par "e" dans une chaîne de caractères:

```Gleam
import regex

string = "Bonjour"
pattern = Regex.make("a")
replaced = Regex.replace_all(pattern, string, "e")
// Output: replaced = "Bonjoure"
```

## Plongée en profondeur

Les expressions régulières peuvent sembler un peu intimidantes au début, mais avec de la pratique, elles deviendront un outil précieux dans votre boîte à outils de programmation. N'hésitez pas à explorer différentes ressources pour comprendre les différents motifs et les utiliser efficacement dans votre code. Vous pouvez également consulter la documentation officielle de Gleam pour plus d'informations sur l'utilisation des expressions régulières dans ce langage.

## Voir aussi

- [Documentation officielle de Gleam](https://gleam.run/)
- [Tutoriel sur les expressions régulières en Gleam](https://dev.to/otterthegreat/advanced-pattern-matching-with-regular-expressions-in-gleam-4bnh)
- [Jeu interactif pour pratiquer les expressions régulières](https://regexone.com/)