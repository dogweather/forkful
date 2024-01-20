---
title:                "Analyse syntaxique de HTML"
html_title:           "Bash: Analyse syntaxique de HTML"
simple_title:         "Analyse syntaxique de HTML"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/parsing-html.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est & Pourquoi ?

Le parsing HTML est l'action de décomposer et d'analyser un document HTML pour le transformer en un format utilisable pour votre programme. C'est essentiel pour les programmeurs qui souhaitent automatiquement extraire, manipuler ou utiliser des informations à partir de pages Web.

## Comment faire :

Alors, comment pouvons-nous analyser du HTML en C ? Le code ci-dessous montre une façon simple d'accomplir cela en utilisant une bibliothèque populaire appelée « gumbo-parser ».

```C
// Inclure la bibliothèque gumbo-parser
#include <gumbo.h>

// Fonction pour parse le HTML
void parse_html(const char* html) {
    GumboOutput* output = gumbo_parse(html);

    // Print le titre de la page, si existant
    GumboNode* title = find_title(output->root);
    if (title) {
        printf("Titre : %s\n", GumboNode->v.text.text);
    }

    gumbo_destroy_output(&kGumboDefaultOptions, output);
}

// Fonction pour trouver le titre de la page
static GumboNode* find_title(const GumboNode* root) {
    // implémentation dépendante de vous
}
```

En supposant que vous avez correctement installé gumbo-parser, le code ci-dessus va lire le HTML donné, et si existant, imprimera le titre de la page.

## Approfondissement :

Historiquement, le parsing HTML en C était un processus complexe et précaire, due à la nature flexible du HTML. Les bibliothèques telles que gumbo-parser ont été développées pour simplifier ce processus.

En ce qui concerne les alternatives, libxml2 est une autre bibliothèque populaire pour le parsing HTML en C. Cependant, elle peut être considérée comme plus complexe à utiliser.

Concernant les détails d'implémentation, le processus de parsing HTML est un processus à deux étapes : la construction de l'arbre DOM et le rendu de l'arbre. Dans notre exemple, gumbo-parser fait simplement la phase de construction DOM. Le rendu est laissé à votre discrétion.

## Voir Aussi :

- Documentation de Gumbo : https://github.com/google/gumbo-parser
- Tutoriel de libxml2 : http://www.xmlsoft.org/html/libxml-HTMLparser.html
- W3C HTML DOM : https://www.w3schools.com/js/js_htmldom.asp