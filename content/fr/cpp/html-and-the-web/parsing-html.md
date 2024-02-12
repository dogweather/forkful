---
title:                "Analyse Syntaxique du HTML"
aliases:
- /fr/cpp/parsing-html.md
date:                  2024-02-03T19:11:54.128179-07:00
model:                 gpt-4-0125-preview
simple_title:         "Analyse Syntaxique du HTML"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Analyser du HTML signifie décomposer le contenu HTML en quelque chose qu'un programme peut comprendre et manipuler. Les programmeurs font cela pour extraire des données, manipuler du contenu ou intégrer de l'extraction web dans leurs applications.

## Comment faire :
C++ n'est pas livré avec des capacités d'analyse HTML intégrées. Vous utiliserez souvent une bibliothèque comme Gumbo-parser de Google, ou quelque chose de similaire. Voici un exemple rapide en utilisant Gumbo-parser :

```C++
#include <iostream>
#include <gumbo.h>

void search_for_links(GumboNode* node) {
    if (node->type != GUMBO_NODE_ELEMENT) {
        return;
    }
    if (node->v.element.tag == GUMBO_TAG_A) {
        GumboAttribute* href = gumbo_get_attribute(&node->v.element.attributes, "href");
        if (href) {
            std::cout << href->value << std::endl;
        }
    }
    GumboVector* children = &node->v.element.children;
    for (unsigned int i = 0; i < children->length; ++i) {
        search_for_links(static_cast<GumboNode*>(children->data[i]));
    }
}

int main() {
    const char* html = "<html><body><a href='https://example.com'>Lien</a></body></html>";
    GumboOutput* output = gumbo_parse(html);
    search_for_links(output->root);
    gumbo_destroy_output(&kGumboDefaultOptions, output);
    return 0;
}
```

Exemple de sortie :
```
https://example.com
```

## Exploration Approfondie
Analyser du HTML n'a pas toujours été simple en C++. Historiquement, les programmeurs utilisaient des expressions régulières ou des analyseurs écrits à la main, tous deux sujets à erreurs et lourds à gérer. De nos jours, des bibliothèques robustes comme Gumbo-parser prennent en charge les subtilités de l'analyse, rendant le processus plus facile et fiable.

Parmi les alternatives, citons Tidy, MyHTML, ou même l'intégration de C++ avec BeautifulSoup de Python via la fonction `system` de C++ ou des interpréteurs embarqués.

En termes d'implémentation, ces bibliothèques convertissent le HTML en un arbre de modèles d'objets de document (DOM). Parcourir et manipuler le DOM permet aux utilisateurs d'extraire et de travailler avec des données, comme démontré dans la section Comment faire.

## Voir Aussi
- [Dépôt GitHub de Gumbo-parser](https://github.com/google/gumbo-parser)
- [Liste des bibliothèques d'analyse HTML](https://en.cppreference.com/w/c/experimental/dynamic)
- [Interopérabilité C++ et Python](https://docs.python.org/3/extending/embedding.html)
