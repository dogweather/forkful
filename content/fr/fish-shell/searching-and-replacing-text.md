---
title:    "Fish Shell: Recherche et remplacement de texte"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes programmeur ou développeur, vous savez probablement déjà que la recherche et remplacement de texte est un processus essentiel dans le développement de logiciels. Cela vous permet de rapidement trouver et remplacer des mots ou des phrases dans un code source, ce qui peut vous faire gagner du temps et réduire les erreurs.

## Comment faire

La recherche et le remplacement de texte peuvent sembler déroutants au départ, mais ne vous inquiétez pas, Fish Shell rend cela très facile. Tout d'abord, ouvrez votre terminal et entrez la commande suivante :

```Fish Shell
sed -i 's/texte1/texte2/g' fichier.txt
```

Cette commande utilise le programme "sed", qui est une abréviation de "stream editor", pour chercher et remplacer toutes les occurrences de "texte1" par "texte2" dans le fichier "fichier.txt". Le flag "-i" indique à sed de modifier directement le fichier plutôt que de simplement afficher le résultat sur le terminal.

Si vous voulez remplacer uniquement la première occurrence de "texte1", vous pouvez utiliser la commande suivante :

```Fish Shell
sed -i 's/texte1/texte2/' fichier.txt
```

Il existe également d'autres options telles que la recherche et le remplacement en utilisant des expressions régulières, mais nous n'entrerons pas dans les détails ici.

## Plongée en profondeur

Maintenant que vous savez comment utiliser la commande "sed" pour la recherche et le remplacement de texte, voici quelques informations supplémentaires pour vous aider à mieux comprendre ce processus. Lorsque vous utilisez la commande "sed", vous pouvez utiliser différents délimiteurs pour séparer les termes de recherche et de remplacement. Par exemple, vous pouvez utiliser "/" comme délimiteur, mais si votre texte contient également des "/", il est préférable d'utiliser un autre caractère tel que "#" ou "!". Vous pouvez également utiliser des drapeaux supplémentaires avec la commande "-e" pour plusieurs expressions de recherche ou "-r" pour utiliser des expressions régulières de type "extended".

## Voir aussi

- Article sur la commande sed (https://www.linux.com/topic/desktop/how-use-sed-search-and-replace-text-files/)
- Documentation officielle de Fish Shell (https://fishshell.com/docs/current/cmds/sed.html)
- Courts métrages pour en apprendre davantage sur les expressions régulières (https://www.youtube.com/playlist?list=PLRqwX-V7Uu6YEypLuls7iidwHMdCM6o2w)