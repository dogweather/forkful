---
title:                "Suppression de caractères correspondant à un motif"
aliases:
- fr/fish-shell/deleting-characters-matching-a-pattern.md
date:                  2024-01-20T17:42:06.281833-07:00
model:                 gpt-4-1106-preview
simple_title:         "Suppression de caractères correspondant à un motif"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why?
Supprimer des caractères selon un motif, c'est retirer de notre texte des bouts spécifiques qui suivent une règle précise. Les programmeurs y recourent pour nettoyer des données, transformer des chaînes de caractères, simplifier du contenu, ou pour toute autre tâche nécessitant de la précision textuelle.

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
