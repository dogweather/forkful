---
date: 2024-01-27 20:33:22.214806-07:00
description: "Comment faire : G\xE9n\xE9rer un nombre al\xE9atoire dans Fish peut\
  \ \xEAtre simple, en utilisant la combinaison des utilitaires syst\xE8me et des\
  \ capacit\xE9s du shell. Ci-\u2026"
lastmod: '2024-03-13T22:44:58.317584-06:00'
model: gpt-4-0125-preview
summary: "G\xE9n\xE9rer un nombre al\xE9atoire dans Fish peut \xEAtre simple, en utilisant\
  \ la combinaison des utilitaires syst\xE8me et des capacit\xE9s du shell."
title: "G\xE9n\xE9ration de nombres al\xE9atoires"
weight: 12
---

## Comment faire :
Générer un nombre aléatoire dans Fish peut être simple, en utilisant la combinaison des utilitaires système et des capacités du shell. Ci-dessous, quelques exemples montrent comment générer des nombres aléatoires dans des plages spécifiées.

**Générer un nombre aléatoire entre 0 et 100 :**

```fish
set -l rand_num (random 0 100)
echo $rand_num
```

**Exemple de sortie :**
```fish
42
```

**Générer un nombre aléatoire entre deux nombres quelconques, disons 50 et 150 :**

```fish
set -l min 50
set -l max 150
set -l rand_num (random $min $max)
echo $rand_num
```

**Exemple de sortie :**
```fish
103
```

**Utiliser random pour mélanger une liste :**

Vous voudrez peut-être aussi mélanger aléatoirement les éléments d'une liste. Voici comment vous pouvez le faire :

```fish
set -l my_list A B C D E
random (seq (count $my_list)) | while read i
    echo $my_list[$i]
end
```

**Exemple de sortie :**
```fish
C
A
E
D
B
```

Veuillez noter, la sortie variera à chaque fois que vous exécutez ces commandes en raison de la nature de l'aléatoire.

## Analyse Approfondie
La fonction `random` de Fish Shell offre une interface facile à utiliser pour générer des nombres pseudo-aléatoires. En interne, elle s'appuie sur les utilitaires de génération de nombres aléatoires au niveau du système, offrant une manière portable d'introduire de l'aléatoire dans vos scripts. Cependant, il est essentiel de se rappeler que l'aléatoire fourni par `random` est suffisant pour la plupart des tâches de script mais pourrait ne pas répondre aux exigences de sécurité cryptographique pour les applications nécessitant un degré d'imprévisibilité plus élevé.

Pour des contextes de sécurité à enjeux élevés, envisagez d'utiliser des outils dédiés ou des bibliothèques de programmation conçues à des fins cryptographiques, qui offrent des garanties de randomisation plus fortes. Néanmoins, pour les scripts généraux et les applications où les normes de sécurité les plus élevées pour l'aléatoire ne sont pas une exigence, la fonction `random` de Fish Shell offre une solution pratique et efficace.
