---
title:                "Trouver la longueur d'une chaîne de caractères"
date:                  2024-01-20T17:47:11.905892-07:00
model:                 gpt-4-1106-preview
simple_title:         "Trouver la longueur d'une chaîne de caractères"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Quoi et Pourquoi ?)
Trouver la longueur d'une chaîne signifie compter le nombre de caractères qu'elle contient. C'est essentiel pour valider des données, trancher des algorithmes ou optimiser des performances.

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