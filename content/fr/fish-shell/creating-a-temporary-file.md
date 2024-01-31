---
title:                "Création d'un fichier temporaire"
date:                  2024-01-20T17:40:21.176089-07:00
model:                 gpt-4-1106-preview
simple_title:         "Création d'un fichier temporaire"

category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Créer un fichier temporaire c'est un peu comme prendre des notes sur une napkin; ça sert à stocker des infos de façon éphémère. Les développeurs font ça pour tester des bouts de code, manipuler des données en transit, ou éviter de surcharger la mémoire vive.

## Comment faire :

```Fish Shell
# Créer un fichier temporaire avec mktemp
set tmpfile (mktemp)
echo "Ceci est un fichier temporaire" > $tmpfile

# Vérifier le contenu
cat $tmpfile

# Sortie attendue:
# Ceci est un fichier temporaire

# N'oubliez pas de supprimer le fichier temporaire à la fin
rm $tmpfile
```

## Exploration en profondeur

Créer des fichiers temporaires n'est pas nouveau; la commande `mktemp` existe depuis les premiers jours d'Unix et reste la méthode standard sous Linux et les systèmes de type Unix. Alternativement, on pourrait aussi rediriger les sorties standards vers `/dev/shm` sous Linux pour un stockage temporaire en mémoire, ou utiliser des noms de fichiers hardcodés pour des scripts simples mais attention aux conflits.

Fish Shell, avec sa syntaxe épurée, facilite ces opérations et rend les scripts plus lisibles. Cependant, `mktemp` est un programme externe, pas une fonctionnalité native de Fish. Le fichier temporaire est souvent stocké dans `/tmp`, un emplacement spécial dans le système de fichiers conçu pour des fichiers qui ne sont pas destinés à rester longtemps.

## Voir aussi

- Documentation de mktemp : [mktemp(1) - Linux man page](https://linux.die.net/man/1/mktemp)
- Guide utilisateur de Fish Shell : [Fish Documentation](https://fishshell.com/docs/current/index.html)
- Informations sur les systèmes de fichiers temporaires : [Filesystem Hierarchy Standard](https://refspecs.linuxfoundation.org/FHS_3.0/fhs/ch03s18.html)
