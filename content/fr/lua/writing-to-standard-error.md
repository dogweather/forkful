---
title:                "Écrire sur la sortie standard"
html_title:           "Lua: Écrire sur la sortie standard"
simple_title:         "Écrire sur la sortie standard"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/lua/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faire?

 "Writing to standard error" en Lua est l'action de transmettre des erreurs ou des messages d'avertissement à l'utilisateur pendant l'exécution d'un programme. Cela peut être utile pour corriger les erreurs et déboguer le code.

## Comment faire:

Pour écrire un message d'erreur ou un avertissement à la sortie standard, utilisez la fonction `io.stderr:write` suivie du message entre parenthèses :

```Lua
io.stderr:write("Erreur : variable invalide. Veuillez réessayer.") -- exemple de message d'erreur
```

Pour écrire un message d'erreur avec un code d'erreur spécifique, utilisez la fonction `error`, en spécifiant le code d'erreur suivi du message entre parenthèses :

```Lua
error(404, "Page introuvable.") -- exemple de message d'erreur avec un code 404
```

## Plongée en profondeur:

Écrire à la sortie standard a été introduit dans Lua dans la version 3.0, et est toujours utilisé comme moyen de déboguer et d'indiquer les erreurs dans le code. Il existe d'autres façons d'effectuer cette tâche, comme utiliser la bibliothèque `debug`, mais écrire à la sortie standard reste une pratique courante pour les programmeurs Lua.

## Voir aussi:

Pour plus d'informations sur l'utilisation de `io.stderr:write` et d'autres fonctions associées, consultez la documenta