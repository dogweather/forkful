---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:28.123294-07:00
description: "Comment faire : Fish Shell utilise des commandes externes comme `date`\
  \ pour obtenir la date actuelle, offrant la flexibilit\xE9 de formater la sortie\
  \ selon\u2026"
lastmod: '2024-03-13T22:44:58.336104-06:00'
model: gpt-4-0125-preview
summary: "Fish Shell utilise des commandes externes comme `date` pour obtenir la date\
  \ actuelle, offrant la flexibilit\xE9 de formater la sortie selon les besoins."
title: Obtenir la date actuelle
weight: 29
---

## Comment faire :
Fish Shell utilise des commandes externes comme `date` pour obtenir la date actuelle, offrant la flexibilité de formater la sortie selon les besoins. Voici comment l'utiliser :

```fish
# Afficher la date actuelle dans le format par défaut
echo (date)

# Exemple de sortie : Wed 25 Oct 2023 15:42:03 BST
```

Pour personnaliser le format de la date, vous pouvez utiliser l'option `+` suivie des spécificateurs de format :

```fish
# Afficher la date actuelle au format AAAA-MM-JJ
echo (date "+%Y-%m-%d")

# Exemple de sortie : 2023-10-25
```

Pour des tâches plus complexes, comme travailler avec des horodatages ou effectuer de l'arithmétique de dates, Fish Shell s'appuie sur des outils externes comme `date` en raison de sa nature de script. Voici un exemple pour obtenir l'horodatage UNIX actuel :

```fish
# Obtenir l'horodatage UNIX actuel
echo (date "+%s")

# Exemple de sortie : 1666710123
```

Et pour ajouter un jour à la date actuelle en utilisant `date` :

```fish
# Ajouter un jour à la date actuelle
echo (date -d "+1 day" "+%Y-%m-%d")

# Exemple de sortie : 2023-10-26
```

Note : Les exemples utilisent des options de la commande `date` qui fonctionnent avec les GNU coreutils. Les options peuvent varier dans d'autres environnements comme macOS, qui utilise par défaut la commande date de BSD. Référez-vous toujours à `date --help` ou à la page de manuel pour les détails spécifiques à votre environnement.
