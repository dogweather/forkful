---
date: 2024-01-20 17:41:31.463382-07:00
description: "Supprimer des caract\xE8res correspondant \xE0 un motif, c'est filtrer\
  \ un texte pour ne garder que ce qui est utile. Les programmeurs le font pour nettoyer\
  \ des\u2026"
lastmod: '2024-03-13T22:44:57.971607-06:00'
model: gpt-4-1106-preview
summary: "Supprimer des caract\xE8res correspondant \xE0 un motif, c'est filtrer un\
  \ texte pour ne garder que ce qui est utile."
title: "Suppression de caract\xE8res correspondant \xE0 un motif"
weight: 5
---

## Ce qu'on fait et pourquoi ?

Supprimer des caractères correspondant à un motif, c'est filtrer un texte pour ne garder que ce qui est utile. Les programmeurs le font pour nettoyer des données, extraire des informations, ou formater des entrées/sorties.

## Comment faire :

```Bash
# Supprimer toutes les occurrences du caractère 'a' dans une chaîne
echo "banana" | tr -d 'a'
# Sortie: bnn

# Retirer les chiffres d'un texte
echo "R2D2 et C3PO" | tr -d '0-9'
# Sortie: RDet CPO

# Utiliser une expression régulière avec sed pour supprimer des motifs spécifiques
echo "Les voitures coûtent 15,000 euros en moyenne" | sed 's/[0-9,]*//g'
# Sortie: Les voitures coûtent  euros en moyenne

# Supprimer les espaces en début et en fin de ligne avec xargs
echo "    du texte entouré d'espaces    " | xargs
# Sortie: du texte entouré d'espaces
```

## Approfondissement

La suppression de caractères selon un motif est une tâche courante dans le traitement de texte et la manipulation de chaînes de caractères. Historiquement, des outils comme `tr`, `sed` ou `awk` étaient utilisés pour ces opérations en UNIX. Ces commandes sont puissantes et peuvent gérer des expressions régulières, offrant ainsi une grande flexibilité.

Outre `tr` et `sed`, il existe d'autres alternatives comme `awk`, `grep` - avec l'option `-o` pour ne conserver que les parties correspondantes - et les capacités natives du shell Bash (e.g., parameter expansion) pour traiter des chaînes de caractères.

Il est important de comprendre les différentes possibilités offertes par ces outils. `tr` est idéal pour supprimer ou remplacer des caractères simples. `sed` est plus adapté aux motifs complexes grâce à ses expressions régulières. D'autres langages de programmation proposent aussi des solutions intégrées, souvent plus intuitives à utiliser.

## Voir aussi

- La documentation officielle de `tr`: https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html
- Une introduction à `sed`: https://www.gnu.org/software/sed/manual/sed.html
- Les bases d'`awk`: https://www.gnu.org/software/gawk/manual/gawk.html
- Guide sur les expressions régulières : https://www.regular-expressions.info/
