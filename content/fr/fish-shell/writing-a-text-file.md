---
title:                "Écriture d'un fichier texte"
html_title:           "Arduino: Écriture d'un fichier texte"
simple_title:         "Écriture d'un fichier texte"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
Écrire un fichier texte, c'est enregistrer des données sous forme de texte. Les programmeurs le font pour sauvegarder des configurations, des scripts ou enregistrer des données de manière simple.

## How to:
Création et écriture dans un fichier:

```Fish Shell
echo "Salut, Fish!" > hello_fish.txt
```

Ajout de contenu à un fichier existant:

```Fish Shell
echo "Encore quelque chose à dire..." >> hello_fish.txt
```

Vérification du contenu:

```Fish Shell
cat hello_fish.txt
```

Sortie:

```
Salut, Fish!
Encore quelque chose à dire...
```

## Deep Dive
Originalement conçu pour Unix, écrire dans un fichier est une opération fondamentale disponible dans tous les systèmes d'exploitation modernes. Alternatives: `vim`, `nano`, ou des redirections dans d'autres shells comme `bash`. En Fish, `>` crée ou écrase un fichier, tandis que `>>` ajoute au fichier sans effacer.

## See Also
- Documentation officielle de Fish Shell : [fishshell.com](https://fishshell.com/docs/current/index.html)
- Tutoriel sur la redirection dans Fish : [https://fishshell.com/docs/current/tutorial.html#tut_redirection](https://fishshell.com/docs/current/tutorial.html#tut_redirection)
- Le manuel de commandes Unix/Linux pour la comparaison : [https://linux.die.net/man/](https://linux.die.net/man/)