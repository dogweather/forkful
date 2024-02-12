---
title:                "Création d'un fichier temporaire"
aliases: - /fr/bash/creating-a-temporary-file.md
date:                  2024-01-20T17:39:54.529152-07:00
model:                 gpt-4-1106-preview
simple_title:         "Création d'un fichier temporaire"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## What & Why? (Quoi & Pourquoi ?)

Créer un fichier temporaire, c'est comme faire une note rapide pour ne pas oublier quelque chose. Les programmeurs les utilisent pour stocker des données de manière éphémère, par exemple, pendant le traitement d'un script.

## How to (Comment faire) :

Voici deux façons simples de créer un fichier temporaire en Bash :

```Bash
# Avec mktemp
tempfile=$(mktemp)
echo "Ceci est un fichier temporaire" > "$tempfile"
echo "Créé : $tempfile"
cat "$tempfile"
```
Sortie :
```
Créé : /tmp/tmp.IkZIXca2Vu
Ceci est un fichier temporaire
```

```Bash
# Directement avec une redirection vers un fichier temporaire
echo "Ceci est un fichier temporaire" > /tmp/monfichier.$$
```

## Deep Dive (Plongée en profondeur) :

Historiquement, la création de fichiers temporaires était gérée manuellement, ce qui posait des problèmes de sécurité et de concurrence. `mktemp` a été introduit pour créer de manière sécurisée des fichiers temporaires uniques. Il garantit qu'aucun autre processus ne crée un fichier avec le même nom, évitant les collisions.

Il y a d'autres alternatives comme `tempfile` (déprécié) ou la création manuelle d'un fichier temporaire en utilisant le PID (Process ID) du script avec `$$` pour l'unicité.

Les fichiers temporaires doivent idéalement être détruits après utilisation. Avec `mktemp`, on peut nettoyer le fichier temporaire en cas de sortie normale ou d'interruption du script avec un piège (trap):

```Bash
tempfile=$(mktemp)
trap "rm -f $tempfile" EXIT

# Utilisez le fichier temporaire ici.

# Le fichier temporaire sera supprimé à la sortie du script.
```

## See Also (Voir aussi) :

- La man page de mktemp pour Linux : [mktemp(1) - Linux man page](https://linux.die.net/man/1/mktemp)
- La documentation sur les signaux et les pièges en Bash : [Bash Trap Command](https://www.linuxjournal.com/content/bash-trap-command)
- Une discussion sur Stack Overflow sur la gestion des fichiers temporaires : [How do I create a temporary file in a secure manner?](https://stackoverflow.com/questions/2794016/how-do-i-create-a-temporary-file-in-a-secure-manner)
