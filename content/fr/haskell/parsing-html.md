---
title:                "Analyse syntaxique de HTML"
html_title:           "Bash: Analyse syntaxique de HTML"
simple_title:         "Analyse syntaxique de HTML"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/parsing-html.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est & pourquoi?
L'analyse HTML consiste à décomposer et à comprendre le code HTML. Les programmeurs le font pour extraire des données spécifiques, modéliser les données sous-jacentes ou manipuler la structure d'un site Web.

## Comment faire:
Pour ce faire, on peut utiliser la librairie "tagsoup" en Haskell. Voyons un exemple:

```Haskell
import Text.HTML.TagSoup

extraireLien :: String -> [String]
extraireLien html = [lien | TagOpen "a" atts <- parseTags html, 
                      ("href", lien) <- atts ]

main = do
  contenu <- readFile "test.html"
  print $ extraireLien contenu
```

Lorsqu'on exécute cela sur un fichier `test.html`, on obtient une liste de tous les liens contenus dans le fichier.

## Plongée Profonde
L'analyse HTML était plus couramment utilisée avant l'arrivée des API modernes qui fournissent des données JSON plus faciles à manipuler. Cependant, il reste encore d'énormes quantités de données disponibles uniquement en HTML. La librairie "tagsoup" est l'un des outils en Haskell pour l'analyse HTML, mais il y a aussi "html-conduit" et "hxt" qui fournissent des fonctionnalités plus avancées. En ce qui concerne les détails de mise en œuvre, "tagsoup" ignore délibérément les erreurs de syntaxe HTML pour faciliter le travail avec du HTML mal formé, ce qui est très fréquent sur le web.

## Voir aussi
- [Documentation de TagSoup](https://hackage.haskell.org/package/tagsoup)
- [Librairie html-conduit](https://hackage.haskell.org/package/html-conduit)
- [Librairie HXT](https://hackage.haskell.org/package/hxt)