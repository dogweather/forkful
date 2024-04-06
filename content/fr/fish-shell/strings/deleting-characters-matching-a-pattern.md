---
date: 2024-01-20 17:42:06.281833-07:00
description: "How to: Historiquement, la manipulation de texte en ligne de commande\
  \ est domin\xE9e par des outils comme `sed` et `awk`. Fish Shell apporte une int\xE9\
  gration\u2026"
lastmod: '2024-04-05T21:53:59.705707-06:00'
model: gpt-4-1106-preview
summary: "Historiquement, la manipulation de texte en ligne de commande est domin\xE9\
  e par des outils comme `sed` et `awk`."
title: "Suppression de caract\xE8res correspondant \xE0 un motif"
weight: 5
---

## How to:
```Fish Shell
# Pour supprimer toutes les instances de "abc" dans un texte
echo "Salutation abcs et abc déclaration abc fin" | string replace -a "abc" ""
# Résultat : Salutation s et  déclaration  fin

# Utiliser un motif avec une expression régulière pour supprimer les chiffres
echo "R2D2 et C3PO sont dans Star Wars 12345" | string replace -ra "[0-9]+" ""
# Résultat : R2D2 et C3PO sont dans Star Wars 

# Supprimer uniquement la première instance d'une chaîne de caractères
echo "Erreur 404: Erreur non trouvée" | string replace "Erreur" ""
# Résultat :  404: Erreur non trouvée
```

## Deep Dive
Historiquement, la manipulation de texte en ligne de commande est dominée par des outils comme `sed` et `awk`. Fish Shell apporte une intégration native avec `string`, qui réduit le besoin de se rappeler de syntaxes complexes. Alternativement, on pourrait utiliser `sed` ou `awk` dans Fish, mais `string` est plus direct et facile à saisir pour des tâches courantes. Niveau performance, `string` est généralement suffisamment rapide pour la plupart des besoins, mais pour des opérations massives de fichiers très volumineux, des outils spécialisés pourraient être plus efficaces.

## See Also
- Documentation Fish pour `string`: https://fishshell.com/docs/current/cmds/string.html
- Tutoriel sur les regex dans Fish: https://fishshell.com/docs/current/tutorial.html#tut_regexes
- Comparaison entre `sed`, `awk` et `string`: https://www.baeldung.com/linux/string-vs-sed-awk
