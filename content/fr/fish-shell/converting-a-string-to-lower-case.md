---
title:                "Fish Shell: Convertir une chaîne en minuscules"
simple_title:         "Convertir une chaîne en minuscules"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Pourquoi

La conversion d'une chaîne de caractères en minuscules est une tâche courante dans la programmation, en particulier lors du traitement de données utilisateur. Il est important de savoir comment le faire efficacement dans Fish Shell pour simplifier votre travail de programmation.

## Comment faire

Dans Fish Shell, la commande `string tolower` peut être utilisée pour convertir une chaîne de caractères en minuscules. Voici un exemple de code pour montrer son utilisation:

```
set my_string "Fish Shell"
echo (string tolower $my_string)
```

La valeur de `my_string` sera convertie en minuscules et affichée comme "fish shell".

## Plongée en profondeur

En plongeant en profondeur dans cette tâche, il est important de comprendre comment Fish Shell gère les caractères accentués lors de la conversion en minuscules. Par exemple, la lettre "é" en majuscule sera convertie en "é" en minuscule, mais la lettre "E" avec un accent aigu sera convertie en "e" en minuscule. Cela peut entraîner des problèmes si vous devez traiter des données multilingues. Dans ce cas, il est recommandé d'utiliser la fonction `string lower` qui prend en compte les règles de casse spécifiques à chaque langue.

## Voir aussi

- [Documentation de Fish Shell sur la commande `string tolower` (en anglais)](https://fishshell.com/docs/current/cmds/string.html#string-tolower)
- [Documentation de Fish Shell sur la fonction `string lower` (en anglais)](https://fishshell.com/docs/current/cmds/string.html#string-lower)
- [Tutoriel sur l'utilisation de Fish Shell (en français)](https://hub.packtpub.com/francais-comment-installer-et-utiliser-fish-shell-sur-linux/)