---
title:                "Analyse de HTML"
html_title:           "Haskell: Analyse de HTML"
simple_title:         "Analyse de HTML"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/parsing-html.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est & Pourquoi?

Parser (analyser) du HTML est une pratique courante dans la programmation. Le HTML est le langage de balisage standard utilisé pour créer des pages web. Les programmeurs utilisent le parsing HTML pour extraire des données spécifiques d'une page web et les manipuler dans leur code.

## Comment faire:

**Exemple 1: Extraire le titre d'une page web en utilisant le module Text.HTML.TagSoup**

```Haskell
import Text.HTML.TagSoup

main = do
  -- Charger une page web
  tags <- getTags "https://www.exemple.com/"
  -- Extraire le contenu de la balise <title>
  let title = fromTagText $ head $ dropWhile (~/= "<title>") tags
  print title
```
Output: "Exemple - Site officiel"

**Exemple 2: Récupérer une liste de liens à partir d'une page web en utilisant le module Text.HTML.TagSoup**

```Haskell
import Text.HTML.TagSoup

main = do
  -- Charger une page web
  tags <- getTags "https://www.exemple.com/links"
  -- Extraire les balises <a> contenant les liens
  let links = [fromAttrib "href" tag | tag <- tags, tag ~== "<a>"]
  print links
```
Output: ["https://www.exemple.com/page1", "https://www.exemple.com/page2", "https://www.exemple.com/page3"]

## Profondeur:

**Contexte historique:**

Le parsing HTML a été présent dès les premiers navigateurs web, mais il a évolué avec le temps. À l'origine, les navigateurs étaient moins stricts en matière de syntaxe HTML et pouvaient donc parser des pages web avec une grande variété de balises. Mais avec l'avènement des normes web et des balises plus complexes, le parsing HTML est devenu un défi pour les programmeurs.

**Alternatives:**

Il existe plusieurs outils et bibliothèques en Haskell pour effectuer le parsing HTML. Outre le module Text.HTML.TagSoup, on peut également utiliser le module Text.XML.Light pour parser des documents XML ou XHTML, ou encore le module Text.HTML.Parser pour une approche plus bas niveau.

**Détails de mise en œuvre:**

Le parsing HTML en Haskell utilise généralement des expressions régulières pour filtrer les balises et les attributs des pages web. Le module Text.HTML.TagSoup fournit également des fonctions pour manipuler facilement les données extraites, comme la recherche de balises spécifiques ou la modification du contenu des balises.

## Voir aussi:

- [Documentation officielle du module Text.HTML.TagSoup](https://hackage.haskell.org/package/tagsoup-0.14.8/docs/Text-HTML-TagSoup.html)
- [Tutoriel pour le parsing HTML en Haskell](https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/text-manipulation/tagsoup)
- [Comparatif de différentes bibliothèques de parsing en Haskell](https://wiki.haskell.org/XML/Introduction)