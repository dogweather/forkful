---
title:                "Fish Shell: Extraction de sous-chaînes"
simple_title:         "Extraction de sous-chaînes"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous travaillez avec Fish Shell, vous savez certainement que l'extraction de sous-chaînes est une tâche courante lors de la manipulation de données. Dans cet article, nous allons explorer pourquoi cette fonctionnalité est utile et comment l'utiliser efficacement.

## Comment faire

Pour extraire une sous-chaîne en utilisant Fish Shell, il suffit d'utiliser la commande `string sub`. Voici un exemple de code avec une chaîne de caractères et les différentes sous-chaînes extraites :

```Fish Shell
set mystring "Bonjour tout le monde"
string sub $mystring 4 7
# Output : jour
string sub $mystring 17 20
# Output : monde
string sub $mystring 8
# Output : tout le monde
```

Notez que vous pouvez spécifier la position de départ et de fin de l'extraction ou simplement indiquer la position de départ pour extraire toute la fin de la chaîne.

## Plongée en profondeur

L'extraction de sous-chaînes peut être utilisée dans de nombreux cas différents, comme la manipulation de noms de fichiers ou de répertoires, le nettoyage de données ou la simplification des tâches répétitives. Vous pouvez également combiner cette fonctionnalité avec d'autres commandes de manipulation de chaînes pour obtenir des résultats plus précis. Par exemple, vous pouvez extraire une sous-chaîne spécifique entre deux mots-clés en utilisant `string match` et `string sub` ensemble.

## Voir aussi

- [La documentation officielle de Fish Shell](https://fishshell.com/docs/current/index.html)
- [Plus de commandes pour manipuler les chaînes en Fish Shell](https://www.shellhacks.com/fish-shell-manipulate-strings/)
- [Un guide complet sur l'utilisation de Fish Shell](https://hackercodex.com/guide/fish-shell/)