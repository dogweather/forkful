---
title:                "Fish Shell: Recherche et remplacement de texte"
simple_title:         "Recherche et remplacement de texte"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Pourquoi

La programmation informatique est un monde en constante évolution et il est essentiel de s'adapter à ces changements. La recherche et le remplacement de texte sont des tâches courantes dans la programmation, et il est important de savoir comment les effectuer efficacement. Heureusement, le Fish Shell propose des fonctionnalités pratiques pour faciliter cette tâche.

## Comment faire

Pour effectuer une recherche et un remplacement de texte dans Fish Shell, vous devez utiliser la commande `sed`. Ci-dessous, vous trouverez des exemples de code et des résultats d'output pour vous aider à comprendre comment utiliser cette commande.

```
Fish Shell
> echo "J'aime écrire du code." | sed 's/J'aime/J'adore/'
```

Résultat de l'output : "J'adore écrire du code."

Dans cet exemple, nous avons utilisé la commande `sed` pour remplacer le mot "J'aime" par "J'adore". La syntaxe utilisée est `sed 's/texte1/texte2/'`, où "texte1" est le texte que vous souhaitez remplacer et "texte2" est le texte par lequel vous souhaitez le remplacer.

Nous pouvons également utiliser la commande `sed` pour remplacer un mot ou une phrase spécifique dans un fichier. Par exemple, si nous voulons remplacer toutes les occurrences de "chat" par "chien" dans un fichier texte nommé "animaux.txt", voici comment nous pouvons le faire :

```
Fish Shell
> sed -i 's/chat/chien/g' animaux.txt
```

Dans cet exemple, nous avons utilisé l'option `-i` pour spécifier que nous voulons modifier le fichier directement. L'option `g` est utilisée pour indiquer que nous voulons remplacer toutes les occurrences du texte, pas seulement la première.

## Deep Dive

La commande `sed` peut sembler un peu complexe au premier abord, mais elle est en fait assez simple à utiliser une fois que vous en comprenez les bases. En plus de ses fonctionnalités de base, elle offre également des options avancées pour affiner votre recherche et remplacement de texte. Par exemple, vous pouvez utiliser des expressions régulières pour effectuer vos recherches. En utilisant l'option `-E`, vous pouvez rechercher des patterns plutôt que du texte exact.

En outre, il est possible de combiner plusieurs options pour une commande `sed`. Par exemple, vous pouvez utiliser `-i` avec `-E` pour effectuer un remplacement dans un fichier tout en utilisant des expressions régulières.

## Voir aussi

- La documentation officielle de Fish Shell : https://fishshell.com/docs/current/
- Un guide complet sur l'utilisation de la commande `sed` : https://www.digitalocean.com/community/tutorials/how-to-use-the-sed-command-on-linux
- Des tutoriels vidéo sur la recherche et le remplacement de texte avec Fish Shell : https://www.youtube.com/playlist?list=PLzfrg3cQjV4gIDWjMaTTvKjKq3FQfbmhk