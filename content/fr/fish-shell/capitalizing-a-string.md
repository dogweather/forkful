---
title:                "Capitalisation d'une chaîne de caractères"
html_title:           "Fish Shell: Capitalisation d'une chaîne de caractères"
simple_title:         "Capitalisation d'une chaîne de caractères"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un fanatique du codage ou si vous travaillez souvent avec des données, vous avez probablement rencontré des occasions où vous devez capitaliser des chaînes de caractères. Cela peut se faire pour de multiples raisons comme l'affichage esthétique ou la manipulation de données structurées. Dans cet article, nous allons vous montrer comment capitaliser une chaîne de caractères en utilisant Fish Shell, la version actuelle de l'interpréteur de commandes en ligne de Fish.

## Comment faire

La commande "string capitalize" de Fish Shell vous permet de capitaliser une chaîne de caractères. Voici un exemple de code pour capitaliser une chaîne de caractères en utilisant cette commande :

```Fish Shell
set my_string "bonjour le monde"
echo $my_string | string capitalize
```

La sortie de ce code sera "Bonjour le monde". Comme vous pouvez le voir, cela fonctionne bien même avec des chaînes de caractères qui contiennent des espaces. Vous pouvez également utiliser cette commande avec des variables :

```Fish Shell
set my_name "pierre"
set my_capitalized_name $my_name | string capitalize
echo $my_capitalized_name
```

La sortie sera "Pierre", avec la première lettre en majuscule.

## Plongeons plus profondément

Maintenant que vous savez comment utiliser la commande "string capitalize", laissez-nous vous expliquer un peu plus comment cela fonctionne en coulisses. Cette commande utilise la fonction correspondante de la bibliothèque standard de Fish, qui utilise la fonction "toupper" de la bibliothèque standard du système. En utilisant cette commande, vous pouvez également spécifier le langage que vous voulez utiliser pour la capitalisation, comme le français ou l'allemand.

Il est également important de noter que cette commande ne modifie pas la chaîne de caractères originale, mais crée une nouvelle chaîne de caractères avec la première lettre en majuscule. Cela signifie que vous devriez stocker le résultat dans une nouvelle variable si vous souhaitez l'utiliser ultérieurement.

## Voir aussi

Pour en savoir plus sur la commande "string capitalize" et d'autres commandes utiles de Fish Shell, consultez les liens suivants :

- [La documentation officielle de Fish Shell](https://fishshell.com/docs/current/cmds/string.html#capitalize)
- [Un guide complet pour maîtriser Fish Shell](https://the.exa.website/shell/fish/)
- [Des conseils pour travailler plus efficacement avec Fish Shell](https://shopify.github.io/shopify/packaging-in-fish-shell/)

Maintenant que vous avez appris à capitaliser des chaînes de caractères en utilisant Fish Shell, pourquoi ne pas explorer davantage cette puissante version de l'interpréteur de commandes ? Vous serez surpris de tout ce qu'elle peut faire pour améliorer votre flux de travail de codage et de gestion de données.