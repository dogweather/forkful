---
title:                "Analyse syntaxique de HTML"
html_title:           "Bash: Analyse syntaxique de HTML"
simple_title:         "Analyse syntaxique de HTML"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/parsing-html.md"
---

{{< edit_this_page >}}

# Analyse Syntaxique en HTML en C++

## Pourquoi et Quoi ?

Analyser l'HTML, c'est lire et interpréter du code HTML pour en extraire certaines informations. Les programmeurs le font pour obtenir une représentation compréhensible et manipulable des données structurelles d'un site web.

## Comment faire :

Regardons un exemple de base d'extraction de titre d'une page web HTML à l'aide de la bibliothèque Gumbo :

```C++
#include <gumbo.h>
#include <iostream>

void chercher_titre(GumboNode* noeud) {
  if (noeud->type == GUMBO_NODE_ELEMENT &&
      noeud->v.element.tag == GUMBO_TAG_TITLE) {
    GumboNode* title_text = static_cast<GumboNode*>(noeud->v.element.children.data[0]);
    std::cout << title_text->v.text.text << std::endl;
    return;
  }

  GumboVector* enfants = &noeud->v.element.children;
  for (unsigned int i = 0; i < enfants->length; ++i) {
    chercher_titre(static_cast<GumboNode*>(enfants->data[i]));
  }
```

En exécutant ce code, vous obtiendrez le titre de la page HTML entrée.

## Approfondissement

Historiquement, le parsing HTML a été réalisé avec des expressions régulières, mais celles-ci ont leurs limites en raison de la complexité et de la flexibilité du HTML. Les bibilothèques comme Gumbo ont été développées pour fournir des parsers HTML plus robustes.

D'autres alternatives existent également, telles que Beautiful Soup (Python) et html.parser (Python), jsoup (Java), htmlagilitypack (.NET) et d'autres. Chacun a ses propres avantages et inconvénients.

Quant à l'implémentation, les parsers HTML utilisent généralement l'algorithme d'arbre DOM pour parcourir et analyser chaque nœud de l'arbre HTML.

## En savoir plus

Voici quelques liens utiles pour en savoir plus sur l'extraction HTML en C++ :

1. [HTML parsing libraries](https://www.htmlgoodies.com/beyond/webmaster/toolbox/article.php/3888101/HTML-Parsing-Libraries-for-C-Cplusplus.htm)
2. [Gumbo parser Github](https://github.com/google/gumbo-parser)
3. [C++ HTML parser stack overflow discussion](https://stackoverflow.com/questions/126279/c-html-parser-library)