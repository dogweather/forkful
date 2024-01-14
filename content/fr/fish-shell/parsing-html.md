---
title:                "Fish Shell: Analyse de l'html"
simple_title:         "Analyse de l'html"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/parsing-html.md"
---

{{< edit_this_page >}}

## Pourquoi
Si vous êtes un programmeur en herbe ou même un expert chevronné, vous avez probablement entendu parler du langage informatique HTML. Mais saviez-vous qu'il est possible de naviguer dans une page web en utilisant le shell Fish ? Cela peut sembler étrange, car le shell Fish est principalement utilisé pour des tâches de développement en ligne de commande. Mais en utilisant des techniques de parsing, il est possible de récupérer des données importantes à partir de pages web sans avoir à les ouvrir dans un navigateur. Dans cet article, nous allons explorer comment utiliser le shell Fish pour parser du HTML et récupérer des données à partir de pages web.

## Comment faire
Pour parser du HTML en utilisant le shell Fish, nous allons utiliser un plugin appelé `htmlq`. Cela peut être installé en utilisant la commande `fisher install decors/htmlq` sur votre terminal. Une fois que le plugin est installé, nous pouvons utiliser la commande `htmlq` pour récupérer des données à partir d'une page web.

```
Fish Shell - Exemple de code pour utiliser htmlq

# Importer la bibliothèque htmlq
source (brew --prefix fish)/share/doc/htmlq/htmlq.fish

# Utiliser htmlq pour récupérer les données au sein des balises spécifiées
curl -s http://exemple.com/page.html | htmlq --id "titre" --attrib "class" --print text contenu

# Sortie : contenu
```

Dans l'exemple ci-dessus, nous utilisons la commande `htmlq` pour récupérer le contenu de la balise `<titre>` avec l'attribut `class`. Nous pouvons spécifier plus d'un attribut en les séparant avec un espace. La partie `--print text` nous permet de spécifier le type de donnée que nous voulons afficher, dans ce cas, du texte. Enfin, nous spécifions le contenu que nous voulons récupérer avec `contenu`.

## Plongée en profondeur
Maintenant que nous savons comment utiliser le `htmlq` pour récupérer des données à partir d'une page HTML, explorons un peu plus en détails comment cela fonctionne. `htmlq` se base sur la bibliothèque Python `lxml` pour analyser et parser le HTML. Cela signifie que nous pouvons utiliser les sélecteurs XPath pour cibler des éléments spécifiques dans le HTML.

Par exemple, si nous voulons récupérer tous les liens (`<a>`) à partir d'une page web, nous pouvons utiliser la commande suivante :

```
Fish Shell - Exemple de code pour utiliser des sélecteurs XPath

curl -s http://exemple.com/page.html | htmlq --xpath "//a" --print text

# Sortie : href vers le lien
```

En utilisant l'argument `--xpath`, nous pouvons spécifier un sélecteur XPath pour cibler les éléments que nous voulons récupérer. Dans cet exemple, nous ciblons tous les éléments `<a>` et affichons les URL en utilisant `--print text`.

La plupart des utilisations de `htmlq` suivent ce même modèle, en utilisant les sélecteurs XPath pour cibler des éléments spécifiques et `--print` pour spécifier le type de donnée à afficher.

## Voir aussi
- Le site officiel du shell Fish : https://fishshell.com/
- La documentation du plugin htmlq : https://fisherman.github.io/htmlq/
- La documentation de la bibliothèque Python lxml : https://lxml.de/