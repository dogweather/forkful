---
title:                "Mettre en majuscule une chaîne"
aliases:
- fr/fish-shell/capitalizing-a-string.md
date:                  2024-02-03T19:05:21.469262-07:00
model:                 gpt-4-0125-preview
simple_title:         "Mettre en majuscule une chaîne"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Mettre en majuscule une chaîne de caractères signifie la modifier pour que la première lettre soit en majuscule et que le reste de la chaîne soit en minuscule. Il s'agit d'une tâche courante dans le traitement de texte, la normalisation des entrées utilisateur et le formatage des données pour garantir la cohérence ou pour répondre à des critères de formatage spécifiques.

## Comment faire :

Dans Fish Shell, les chaînes peuvent être manipulées directement avec des fonctions intégrées, sans avoir besoin d'outils externes ou de bibliothèques. Pour mettre en majuscule une chaîne de caractères, vous pouvez combiner la commande `string` avec des sous-commandes.

```fish
# Chaîne d'exemple
set sample_string "hello world"

# Mettre en majuscule la première lettre
set capitalized_string (string sub -l 1 -- $sample_string | string upper)(string sub -s 2 -- $sample_string)

echo $capitalized_string
```

Sortie :
```
Hello world
```

Pour des scénarios nécessitant la mise en majuscule de plusieurs mots dans une chaîne (par exemple, convertir "hello world" en "Hello World"), vous itéreriez sur chaque mot, en appliquant la logique de capitalisation à chacun :

```fish
# Phrase d'exemple
set sentence "hello fish shell programming"

# Mettre en majuscule chaque mot
set capitalized_words (string split " " -- $sentence | while read -l word; string sub -l 1 -- $word | string upper; and string sub -s 2 -- $word; end)

# Joindre les mots capitalisés
set capitalized_sentence (string join " " -- $capitalized_words)

echo $capitalized_sentence
```

Sortie :
```
Hello Fish Shell Programming
```

Notez que Fish Shell n'offre pas directement une méthode unique de commande pour la capitalisation complète de phrases de la même manière que certains langages de programmation le font avec leurs méthodes de chaînes. Par conséquent, combiner `string split`, `string sub`, `string upper`, puis les réunir représente une approche idiomatique dans Fish Shell pour atteindre cet objectif.
