---
date: 2024-01-20 17:47:11.905892-07:00
description: How to (Comment faire ?) Avec Fish, c'est simple comme bonjour. Utilisez
  `string length`. Voici un exemple .
lastmod: '2024-04-05T21:53:59.712831-06:00'
model: gpt-4-1106-preview
summary: Utilisez `string length`.
title: "Trouver la longueur d'une cha\xEEne de caract\xE8res"
weight: 7
---

## How to (Comment faire ?)
Avec Fish, c'est simple comme bonjour. Utilisez `string length`. Voici un exemple :

```Fish Shell
# Définir une chaîne
set maChaine "Bonjour le monde!"

# Trouver sa longueur
string length $maChaine
```

Sortie :

```
17
```

## Deep Dive (Plongée en Profondeur)
Historiquement, trouver la longueur d'une chaîne était souvent réalisé en C via la fonction `strlen()`, parcourant chaque caractère jusqu'au terminateur null. En Fish, c'est encapsulé dans la commande `string`, plus sécurisée et plus pratique.

Autres méthodes ? Certains scripts bash utilisent `${#variable}` mais cette syntaxe n'est pas disponible dans Fish qui préfère une approche de commande explicite.

Détails d'implémentation : `string length` compte les caractères Unicode correctement, ainsi un caractère composite est compté comme un seul caractère.

## See Also (Voir Aussi)
- [Documentation Fish pour les commandes string](https://fishshell.com/docs/current/cmds/string.html)
- [Pourquoi compter les caractères ?](https://stackoverflow.com/questions/17343830/why-would-a-program-need-to-count-characters)
- [Comparatif des fonctions de chaîne dans différents shells](https://hyperpolyglot.org/unix-shells#string-operations)
