---
title:                "Análisis sintáctico de html"
html_title:           "Ruby: Análisis sintáctico de html"
simple_title:         "Análisis sintáctico de html"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/parsing-html.md"
---

{{< edit_this_page >}}

## ¿Qué y Por qué?

La interpretación (parsing) del HTML es el proceso de analizar el código HTML para su manipulación y extracción de datos. Los programadores lo hacen para recolectar información o renderizar páginas web en un navegador.

## ¿Cómo hacerlo?

Ejemplo sencillo de cómo leer y parsear un archivo HTML usando la biblioteca Gumbo en C:

```C
#include <stdio.h>
#include <stdlib.h>
#include <gumbo.h>

void busqueda_textos(GumboNode* node) {
    if (node->type == GUMBO_NODE_TEXT) {
        printf("%s\n", node->v.text.text);
    } else if (node->type == GUMBO_NODE_ELEMENT &&
            node->v.element.tag != GUMBO_TAG_SCRIPT &&
            node->v.element.tag != GUMBO_TAG_STYLE) {
        GumboVector* children = &node->v.element.children;
        for (unsigned int i = 0; i < children->length; ++i) {
            busqueda_textos(children->data[i]);
        }
    }
}

int main() {
    GumboOutput* output = gumbo_parse("<h1>¡Hola Mundo!</h1>");
    busqueda_textos(output->root);
    gumbo_destroy_output(&kGumboDefaultOptions, output);
}
```

La salida de este programa será simplemente:

```C
¡Hola Mundo!
```
## Inmersión profunda

La interpretación de HTML data desde los inicios de la web cuando los navegadores tenían que interpretarlo para renderizarlo. Existen varias bibliotecas de interpretación de HTML para diferentes lenguajes. En el caso de C, aparte de Gumbo, algunos otros son htmlcxx, MyHTML y libxml2.

La implementación de la interpretación del HTML implica la lectura y análisis del código, identificando etiquetas, atributos y texto. Después de este análisis, queda desglosado en un árbol de nodos (Dom Tree) facilitando su manipulación.

## Ver también

1. Gumbo: https://github.com/google/gumbo-parser
2. MyHTML: https://github.com/lexborisov/myhtml
3. libxml2: http://xmlsoft.org/examples/index.html
4. htmlcxx: http://htmlcxx.sourceforge.net/