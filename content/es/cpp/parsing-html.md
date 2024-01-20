---
title:                "Análisis de HTML"
date:                  2024-01-20T15:30:41.932786-07:00
html_title:           "Arduino: Análisis de HTML"
simple_title:         "Análisis de HTML"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/parsing-html.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
El análisis de HTML (parsing HTML) implica desgranar y entender el contenido marcado de una página web. Los programadores lo hacen para extraer información, manipular elementos de la página o automatizar interacciones web.

## Cómo hacerlo:

Aquí hay un ejemplo simple utilizando la biblioteca C++ `Gumbo`. Primero, instala la biblioteca:

```bash
sudo apt-get install libgumbo-dev
```

Luego, el código de ejemplo:

```C++
#include <iostream>
#include "gumbo.h"

void search_for_links(GumboNode* node) {
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
                       "<a href=\"http://example.com\">First link</a>"
                       "<a href=\"http://example2.com\">Second link</a>"
                       "</body></html>";
    GumboOutput* output = gumbo_parse(html);
    search_for_links(output->root);
    gumbo_destroy_output(&kGumboDefaultOptions, output);
}
```

Si guardas esto en un archivo llamado `main.cpp` y lo compilas con:

```bash
g++ main.cpp -lgumbo -o htmlparser
```

La salida sería:

```
http://example.com
http://example2.com
```

## Profundización:

Históricamente, el HTML se analizaba con expresiones regulares, pero esto se volvió complicado debido a la naturaleza irregular del HTML. Las bibliotecas modernas, como `Gumbo`, facilitan este proceso utilizando el Análisis Sintáctico (parsing) de HTML basado en el estándar de HTML5.

Alternativas populares en C++ incluyen `libxml2` y `htmlcxx`. En otros lenguajes, `BeautifulSoup` en Python es una opción recurrente, y `jsdom` en JavaScript es común para tareas de scraping en Node.js.

Sobre la ejecución, es clave considerar la robustez del parser frente a HTML mal formado o errores de sintaxis. Los parsers modernos tienden a ser tolerantes a errores y capaces de procesar HTML como lo hacen los navegadores web modernos.

## Ver También:

- Documentación de Gumbo: https://github.com/google/gumbo-parser
- XML y HTML con libxml2: http://xmlsoft.org/html/libxml-HTMLparser.html
- htmlcxx, un parser HTML en C++: http://htmlcxx.sourceforge.net/
- BeautifulSoup para Python: https://www.crummy.com/software/BeautifulSoup/
- `jsdom` para Node.js: https://github.com/jsdom/jsdom