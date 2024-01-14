---
title:                "Fish Shell: Extraction de sous-chaînes"
programming_language: "Fish Shell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Pourquoi

Dans la programmation, il est souvent utile d'extraire des parties spécifiques d'une chaîne de caractères. Cela peut être nécessaire pour traiter des données ou pour effectuer une recherche. Avec Fish Shell, il est facile d'extraire des sous-chaînes de manière efficace et précise.

## Comment faire

La syntaxe pour extraire des sous-chaînes avec Fish Shell est simple. Il suffit d'utiliser le symbole de crochets [ ] et de spécifier l'index ou la plage de caractères à extraire.

Voici un exemple de code pour extraire une sous-chaîne de caractères à partir d'une chaîne donnée :

```Fish Shell
set my_string "Bonjour le monde"
echo $my_string[8-12]
```

La sortie de ce code sera "le". En utilisant les crochets, nous avons spécifié l'index de départ et de fin pour extraire la sous-chaîne souhaitée.

Il est également possible d'utiliser des variables pour définir les index, rendant ainsi cette méthode encore plus flexible. Voici un autre exemple :

```Fish Shell
set my_string "Bonjour le monde"
set index_start 8
set index_end 12
echo $my_string[$index_start-$index_end]
```

La sortie reste la même que précédemment, mais les index ont été définis avec des variables.

## Plongée en profondeur

En plus de spécifier des index précis, il est également possible d'utiliser des caractères spéciaux pour extraire des sous-chaînes avec Fish Shell. Par exemple, le symbole % peut être utilisé pour spécifier le dernier index d'une chaîne. Voici un exemple :

```Fish Shell
set my_string "Bonjour le monde"
echo $my_string[8-%]
```

La sortie de ce code sera "le monde". Le symbole % signifie que nous voulons extraire la sous-chaîne à partir de l'index 8 jusqu'à la fin de la chaîne.

Il est également possible d'utiliser le symbole # pour extraire une sous-chaîne à partir du début de la chaîne. Voici un exemple :

```Fish Shell
set my_string "Bonjour le monde"
echo $my_string[#8-12]
```

La sortie de ce code sera "Bonjour". Le symbole # signifie que nous voulons extraire la sous-chaîne à partir du début de la chaîne jusqu'à l'index 12.

En utilisant ces symboles spéciaux, il est possible de personnaliser davantage l'extraction de sous-chaînes en fonction de nos besoins.

## Voir aussi

Voici quelques liens utiles pour en savoir plus sur l'extraction de sous-chaînes avec Fish Shell :

- Documentation officielle de Fish Shell sur les sous-chaînes : https://fishshell.com/docs/current/cmds/set.html#substrings
- Guide de référence de Fish Shell : https://fishshell.com/docs/current/index.html
- Tutoriel vidéo sur l'utilisation des sous-chaînes avec Fish Shell : https://www.youtube.com/watch?v=brpBN-CzZcs

Avec ces informations, vous êtes maintenant prêt à extraire des sous-chaînes en toute facilité avec Fish Shell !