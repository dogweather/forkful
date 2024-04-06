---
date: 2024-01-20 17:40:21.176089-07:00
description: "Comment faire : Cr\xE9er des fichiers temporaires n'est pas nouveau;\
  \ la commande `mktemp` existe depuis les premiers jours d'Unix et reste la m\xE9\
  thode\u2026"
lastmod: '2024-04-05T21:53:59.744141-06:00'
model: gpt-4-1106-preview
summary: "Cr\xE9er des fichiers temporaires n'est pas nouveau; la commande `mktemp`\
  \ existe depuis les premiers jours d'Unix et reste la m\xE9thode standard sous Linux\
  \ et les syst\xE8mes de type Unix."
title: "Cr\xE9ation d'un fichier temporaire"
weight: 21
---

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
