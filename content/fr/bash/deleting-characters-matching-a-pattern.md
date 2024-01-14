---
title:    "Bash: Suppression de caractères correspondant à un modèle"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/bash/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Pourquoi 
Supprimer des caractères correspondants à un modèle peut être utile pour nettoyer ou filtrer des données. Cela peut également être utile pour automatiser certaines tâches de traitement de texte dans un script Bash.

## Comment procéder
Pour supprimer des caractères correspondants à un motif dans un script Bash, utilisez la commande "sed" suivie du motif à supprimer entre des barres obliques. Par exemple, si vous voulez supprimer tous les "a" de votre texte, vous pouvez utiliser la commande suivante :

```Bash 
sed 's/a//g' fichier.txt 
```
Cela supprimera toutes les occurrences du caractère "a" dans votre fichier texte.

## Plongée en profondeur 
La commande "sed" est un outil très puissant pour effectuer des actions de substitution dans un fichier ou un flux de texte. En plus de supprimer des caractères correspondants à un motif, elle peut également être utilisée pour remplacer ou ajouter du texte pour des tâches de traitement de texte plus complexes.

La syntaxe complète pour la commande "sed" est la suivante :

```Bash
sed 's/pattern/replace/g' fichier.txt 
```
Dans cette commande, "pattern" représente le motif à supprimer, remplacer ou ajouter, et "replace" représente le texte de remplacement ou d'ajout. Le "g" à la fin indique que l'action doit être répétée pour toutes les occurrences du motif dans le fichier.

Il existe également d'autres options pour affiner votre commande "sed", telles que "i" pour insérer du texte avant une ligne correspondant au motif et "d" pour supprimer une ligne correspondant au motif.

## Voir aussi 
- [Documentation sur la commande `sed` en Bash](https://www.tutorialspoint.com/sed/sed_deletion_operation.htm)
- [Guide complet des expressions régulières en Bash](https://www.digitalocean.com/community/tutorials/avec-sed-grep-et-awk-comment-manipuler-et-controller-des-textes-avec-des-expressions-regulieres-en-bash)
- [Exemples pratiques de traitement de texte en Bash avec `sed`](https://www.tecmint.com/sed-command-to-delete-multiple-lines-in-a-file/)