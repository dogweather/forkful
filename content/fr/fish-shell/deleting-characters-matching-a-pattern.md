---
title:                "Fish Shell: Suppression de caractères correspondant à un motif"
programming_language: "Fish Shell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Pourquoi

L'une des fonctionnalités les plus utiles du Fish Shell est la possibilité de supprimer des caractères correspondant à un motif. Que vous souhaitiez nettoyer un fichier de données ou supprimer des lignes indésirables dans votre code, cette fonctionnalité peut vous faire gagner un temps précieux.

## Comment faire

Dans Fish Shell, vous pouvez utiliser la commande `sed` pour supprimer des caractères correspondant à un motif. Par exemple, pour supprimer toutes les lignes contenant "hello" dans un fichier, vous pouvez utiliser la commande suivante :

```Fish Shell
sed '/hello/d' fichier.txt 
```

Cela supprimera toutes les lignes contenant "hello" dans le fichier et affichera le résultat dans le terminal. Vous pouvez également utiliser des expressions régulières pour affiner votre sélection. Par exemple, pour supprimer toutes les lignes contenant un nombre à quatre chiffres, vous pouvez utiliser :

```Fish Shell
sed '/[0-9]\{4\}/d' fichier.txt
```

Assurez-vous de bien maîtriser les expressions régulières avant d'utiliser cette fonctionnalité, car elles peuvent être délicates.

## Plongée en profondeur

La commande `sed` utilise en fait des flux de texte pour supprimer les caractères correspondant à un motif. Cela signifie qu'au lieu de modifier directement le fichier source, elle affiche le résultat modifié dans le terminal. Vous pouvez également utiliser la redirection pour enregistrer le résultat dans un nouveau fichier.

De plus, la commande `sed` peut être utilisée conjointement avec d'autres commandes de manipulation de texte, telles que `grep`, pour créer des expressions plus complexes.

## Voir aussi

- [Les expressions régulières avec Fish Shell](https://fishshell.com/docs/current/regexp.html)
- [La référence de la commande sed](https://fishshell.com/docs/current/cmds/sed.html)
- [Utiliser des pipelines en Fish Shell](https://fishshell.com/docs/current/tutorial.html#using-pipelines)