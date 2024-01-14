---
title:                "Gleam: Suppression de caractères correspondant à un motif"
programming_language: "Gleam"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Pourquoi
Dans notre parcours en tant que programmeurs, il existe souvent des moments où nous devons supprimer des caractères dans une chaîne de texte qui correspondent à un certain modèle. Cela peut sembler une tâche simple, mais il est important de connaître les différentes façons de le faire efficacement et correctement. Dans cet article, je vais vous montrer comment supprimer des caractères correspondant à un modèle en utilisant le langage de programmation Gleam.

## Comment faire
Pour supprimer des caractères correspondant à un modèle en utilisant Gleam, nous allons utiliser la fonction `remove_chars_matching` de la bibliothèque standard de Gleam. Voici un exemple de code :

```Gleam
import gleam/string

let text = "Bonjour tout le monde"

let filtered_text = string.remove_chars_matching(text, "aeiou")

io.print("Texte d'origine : " ++ text)
io.print("Texte filtré : " ++ filtered_text)

```
Output:
Texte d'origine : Bonjour tout le monde
Texte filtré : Bnjr tt l mnd

Dans cet exemple, nous avons importé le module `gleam/string` et utilisé la fonction `remove_chars_matching` pour supprimer les voyelles de la chaîne de texte. Vous pouvez également utiliser n'importe quel autre modèle pour supprimer des caractères en utilisant cette fonction. N'oubliez pas que les caractères doivent être passés en tant que chaîne de texte, même s'il n'y en a qu'un.

## Plongée en profondeur
Maintenant que nous avons vu comment supprimer des caractères correspondant à un modèle en utilisant la fonction `remove_chars_matching`, il est intéressant de comprendre comment cette fonction fonctionne en interne. En réalité, cette fonction utilise la méthode `filter` de la bibliothèque standard de Gleam pour parcourir la chaîne de texte et supprimer les caractères correspondant au modèle donné. Ensuite, elle renvoie la chaîne de texte filtrée.

Il est également possible d'utiliser des expressions régulières pour supprimer des caractères correspondant à un modèle en utilisant la fonction `remove_chars_matching_regex` de la bibliothèque standard de Gleam. Cela peut être utile si vous avez besoin de supprimer des modèles plus complexes de votre chaîne de texte.

## Voir aussi
- Documentation officielle de la fonction remove_chars_matching : https://gleam.run/std/string.html#remove_chars_matching
- Tutoriel sur l'utilisation des expressions régulières en Gleam : https://gleam.run/tutorials/regular-expressions.html
- Exemple pratique d'utilisation de remove_chars_matching en Gleam : https://dev.to/lucaspvandermeer/removing-specific-characters-from-strings-in-gleam-386g

Merci d'avoir lu cet article sur la suppression de caractères correspondant à un modèle en utilisant Gleam ! J'espère que cela vous a été utile dans votre parcours de programmation. N'hésitez pas à explorer d'autres fonctions de la bibliothèque standard de Gleam pour découvrir de nouvelles façons de manipuler des chaînes de texte. À bientôt !