---
title:                "Fish Shell: Recherche et remplacement de texte"
programming_language: "Fish Shell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Pourquoi

La recherche et le remplacement de texte sont des tâches courantes lors de la programmation. Cela peut être nécessaire pour corriger des erreurs d'orthographe, faire des modifications globales ou même pour automatiser certaines tâches. Heureusement, le Fish Shell dispose de fonctionnalités intégrées pour faciliter cette tâche et rendre le processus plus rapide et plus efficace.

## Comment faire

Pour faire des recherches et des remplacements de texte dans le Fish Shell, vous pouvez utiliser la commande `sed`. Par exemple, si vous souhaitez remplacer toutes les occurrences du mot "bonjour" par "salut" dans un fichier, vous pouvez utiliser la commande suivante :

```Fish Shell
sed -i 's/bonjour/salut/g' mon_fichier.txt
```

Dans ce cas, nous utilisons l'option `-i` pour modifier le fichier directement, plutôt que de simplement afficher les résultats à l'écran. La commande `sed` utilise le format `s/pattern/replacement/`, où `pattern` est le texte à rechercher et `replacement` est le texte de remplacement.

Vous pouvez également utiliser des expressions régulières pour une recherche et un remplacement plus complexes. Par exemple, si vous souhaitez remplacer toutes les occurrences d'un nombre à deux chiffres suivi d'un point-virgule par le même nombre suivi de deux points-virgules, vous pouvez utiliser cette commande :

```Fish Shell
sed -i 's/\([0-9]\{2\}\);/\1;;/g' mon_fichier.txt
```

Dans ce cas, nous avons utilisé des parenthèses pour créer un groupe de correspondance avec le nombre à deux chiffres, puis nous l'avons réutilisé dans le texte de remplacement avec `\1`. Les expressions régulières peuvent sembler complexes au début, mais elles peuvent être très utiles pour des recherches et des remplacements précis.

## Plongée en profondeur

La commande `sed` est très puissante et peut être utilisée pour des cas plus avancés, tels que la recherche et le remplacement en utilisant des modèles de lignes entières, des modifications en utilisant des expressions régulières étendues, ou même en combinant plusieurs commandes avec des tubes. Pour en savoir plus sur les différentes options et fonctionnalités disponibles, vous pouvez consulter la documentation officielle du Fish Shell sur la commande `sed`.

## Voir aussi

- [Documentation officielle du Fish Shell pour la commande `sed`](https://fishshell.com/docs/current/cmds/sed.html)
- [Guide sur les expressions régulières pour débutants](https://www.rexegg.com/regex-quickstart.html)
- [Liste des commandes courantes du Fish Shell](https://fishshell.com/docs/current/commands.html)