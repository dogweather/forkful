---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:44.687685-07:00
description: "Comment faire : En Bash, la commande `date` est votre principal outil\
  \ pour obtenir la date et l'heure actuelles. Voici quelques exemples de comment\u2026"
lastmod: '2024-03-13T22:44:58.009023-06:00'
model: gpt-4-0125-preview
summary: En Bash, la commande `date` est votre principal outil pour obtenir la date
  et l'heure actuelles.
title: Obtenir la date actuelle
weight: 29
---

## Comment faire :
En Bash, la commande `date` est votre principal outil pour obtenir la date et l'heure actuelles. Voici quelques exemples de comment l'utiliser :

1. **Obtenir la date et l'heure actuelles au format par défaut :**

```bash
date
```

*Exemple de sortie :*
```
Wed Apr 5 14:22:04 PDT 2023
```

2. **Personnaliser le format de sortie :** Vous pouvez spécifier le format de sortie en utilisant les spécificateurs de format `+%`. Par exemple, pour afficher la date au format AAAA-MM-JJ :

```bash
date "+%Y-%m-%d"
```

*Exemple de sortie :*
```
2023-04-05
```

3. **Obtenir le horodatage UNIX actuel :** L'horodatage UNIX est le nombre de secondes depuis l'Époque Unix (1er janvier 1970). Ceci est utile pour les scripts qui effectuent des calculs basés sur les différences de temps.

```bash
date "+%s"
```

*Exemple de sortie :*
```
1672877344
```

Aucune bibliothèque tierce populaire n'est généralement utilisée pour cette opération de base en Bash car la commande `date` intégrée offre une fonctionnalité complète. Cependant, pour des manipulations de date et d'heure plus avancées, les programmeurs pourraient utiliser d'autres langages de programmation ou outils qui offrent des bibliothèques pour l'arithmétique et l'analyse de dates, tels que le module `datetime` de Python.
