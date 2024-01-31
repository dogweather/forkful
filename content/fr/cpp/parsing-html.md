---
title:                "Analyse syntaxique de HTML"
date:                  2024-01-20T15:30:44.802556-07:00
html_title:           "Arduino: Analyse syntaxique de HTML"
simple_title:         "Analyse syntaxique de HTML"

category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? (Quoi et Pourquoi ?)
Parseur HTML, ça sert à lire et comprendre le HTML par un programme. On le fait pour extraire infos, manipuler les données, ou automatiser des tâches web.

## How to (Comment faire) :
Pour parser l'HTML en C++, on utilise souvent des bibliothèques tierces car le standard ne fournit pas d'outils dédiés. Voici un exemple avec Gumbo-parser.

```C++
#include <iostream>
#include "gumbo.h"

static void search_for_links(GumboNode* node) {
  if (node->type != GUMBO_NODE_ELEMENT) {
    return;
  }
  
  GumboAttribute* href;
  if (node->v.element.tag == GUMBO_TAG_A &&
      (href = gumbo_get_attribute(&node->v.element.attributes, "href"))) {
    std::cout << href->value << std::endl;
  }

  GumboVector* children = &node->v.element.children;
  for (unsigned int i = 0; i < children->length; ++i) {
    search_for_links(static_cast<GumboNode*>(children->data[i]));
  }
}

int main() {
  const char* html = "<html><body>"
                     "<a href=\"http://example.com\">Example</a>"
                     "</body></html>";
  GumboOutput* output = gumbo_parse(html);
  search_for_links(output->root);
  gumbo_destroy_output(&kGumboDefaultOptions, output);
}
```
Sortie :
```
http://example.com
```
## Deep Dive (Plongée en profondeur) :
Historiquement, parser de l'HTML en C++ était compliqué, les bibliothèques comme libxml++ ou BeautifulSoup pour Python étaient plus utilisées. Alternativement, on a Gumbo-parser de Google ou Htmlcxx. Choisir la bonne lib dépend de vos besoins : rapidité, compatibilité, facilité d'utilisation. Une implémentation efficace requiert une gestion correcte de la mémoire et du traitement des exceptions XML/HTML mal formées.

## See Also (Voir aussi) :
- [Gumbo-parser GitHub](https://github.com/google/gumbo-parser)
- [libxml++](http://libxmlplusplus.sourceforge.net/)
- [Htmlcxx](http://htmlcxx.sourceforge.net/)
