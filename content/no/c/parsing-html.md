---
title:                "Analysering av html"
html_title:           "C#: Analysering av html"
simple_title:         "Analysering av html"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/parsing-html.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å parse HTML handler om å bryte ned HTML-kode i mindre deler for å forstå dens struktur og innhold. Programmerere utfører denne handlingen for å ekstrahere spesifikk informasjon, manipulere data, og bygge dynamiske nettsider i realtid.

## Hvordan:

Her er et eksempel på hvordan å lage en enkel HTML-parser i C:

```C
#include <stdio.h>
#include "gumbo.h"

void print_tree( GumboNode* node ) {
    if (node->type != GUMBO_NODE_ELEMENT) {
        return;
    }
    GumboVector* children = &node->v.element.children;
    for (unsigned int i = 0; i < children->length; ++i) {
        print_tree(children->data[i]);
    }
}

int main() {
    GumboOutput* output = gumbo_parse("<h1>Hello, World!</h1>");
    print_tree(output->root);
    gumbo_destroy_output(&kGumboDefaultOptions, output);
    return 0;
}
```

Da du kjører denne koden, vil utdata se slik ut:

```
Hello, World!
```

Du vil ikke se HTML-taggene, men teksten blir ekstrahert.

## Dypdykk

Gumbo er en åpen kildekode C HTML-parser bygget av Google. Historisk sett har det vært få gode alternativer for HTML-parsing i C, men de nyere alternativene har blitt mer fremtredende, som MyHTML og HTMLParserC.

Parsing av HTML kan variere i kompleksitet, avhengig av hvor robust parseren er. Det krever forståelse av Document Object Model (DOM), som er en kryssplattform og språkuavhengig grensesnitt som lar programmer manipulere HTML.

## Se Også

1. Gumbo HTML-parser: https://github.com/google/gumbo-parser
2. MyHTML GitHub: https://github.com/lexborisov/myhtml
3. HTMLParserC GitHub: https://github.com/AMDmi3/htmlparser
4. Document Object Model (DOM): https://developer.mozilla.org/no/docs/Web/API/Document_Object_Model/Introduction