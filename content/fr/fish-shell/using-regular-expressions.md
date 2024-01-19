---
title:                "Utiliser les expressions régulières"
html_title:           "C: Utiliser les expressions régulières"
simple_title:         "Utiliser les expressions régulières"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Les expressions régulières sont des séquences de caractères qui définissent un modèle de recherche. Elles permettent aux programmeurs d'analyser et de manipuler des chaînes de caractères de manière efficace et précise.

## Comment faire :

```Fish Shell
# Cherchez une correspondance exacte
grep 'exemples' fichier.txt

# Cherchez une correspondance de modèle
grep -E 'exemples|modèles' fichier.txt

# Remplacer une chaîne de caractères
echo 'Hello, World!' | sed 's/World/Monde/g'
```

Ce qui donnera respectivement :

```Fish Shell
# des exemples
# des exemples
# des modèles
# Hello, Monde!
```

## Plongée en Profondeur :

Les expressions régulières existent depuis les années 1950 et ont été développées initialement pour le langage de programmation Lisp. Il existe plusieurs alternatives, y compris globbing (pour des modèles plus simples) et des moteurs d'expression régulière plus performants comme PCRE. Sous Fish Shell, `grep` et `sed` sont intégrés et prennent en charge les expressions régulières par défaut.

## Voir Aussi :

Pour approfondir votre connaissance sur les expressions régulières, vous pouvez consulter les ressources suivantes :

- Le guide officiel de `Fish Shell` sur Comment utiliser les expressions régulières : http://fishshell.com/docs/current/index.html#regular-expressions 
- Le site web Regular-Expressions.info fournit une ressource complète pour la syntaxe et l'utilisation des expressions régulières : https://www.regular-expressions.info/
- Un tutoriel interactif pour apprendre les expressions régulières : https://regexone.com/