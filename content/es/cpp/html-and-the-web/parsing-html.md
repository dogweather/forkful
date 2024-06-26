---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:11:24.028084-07:00
description: "C\xF3mo hacerlo: C++ no viene con capacidades incorporadas para el an\xE1\
  lisis de HTML. A menudo se utiliza una biblioteca como Gumbo-parser de Google, o\
  \ algo\u2026"
lastmod: '2024-03-13T22:44:59.373203-06:00'
model: gpt-4-0125-preview
summary: "C++ no viene con capacidades incorporadas para el an\xE1lisis de HTML."
title: Analizando HTML
weight: 43
---

## Cómo hacerlo:
C++ no viene con capacidades incorporadas para el análisis de HTML. A menudo se utiliza una biblioteca como Gumbo-parser de Google, o algo similar. Aquí tienes un ejemplo rápido usando Gumbo-parser:

```C++
#include <iostream>
#include <gumbo.h>

void buscar_enlaces(GumboNode* nodo) {
    if (nodo->type != GUMBO_NODE_ELEMENT) {
        return;
    }
    if (nodo->v.element.tag == GUMBO_TAG_A) {
        GumboAttribute* href = gumbo_get_attribute(&nodo->v.element.attributes, "href");
        if (href) {
            std::cout << href->value << std::endl;
        }
    }
    GumboVector* children = &nodo->v.element.children;
    for (unsigned int i = 0; i < children->length; ++i) {
        buscar_enlaces(static_cast<GumboNode*>(children->data[i]));
    }
}

int main() {
    const char* html = "<html><body><a href='https://example.com'>Enlace</a></body></html>";
    GumboOutput* output = gumbo_parse(html);
    buscar_enlaces(output->root);
    gumbo_destroy_output(&kGumboDefaultOptions, output);
    return 0;
}
```

Salida de muestra:
```
https://example.com
```

## Estudio profundo
Analizar HTML no siempre ha sido sencillo en C++. Históricamente, los programadores utilizarían expresiones regulares o analizadores escritos a mano, ambos propensos a errores y engorrosos. Hoy en día, bibliotecas robustas como Gumbo-parser manejan las complejidades del análisis, haciendo que sea más fácil y fiable.

Las alternativas incluyen Tidy, MyHTML, o incluso integrar C++ con BeautifulSoup de Python a través de la función `system` de C++ o intérpretes embebidos.

En términos de implementación, estas bibliotecas convierten HTML en un árbol del Modelo de Objeto de Documento (DOM). Recorrer y manipular el DOM permite a los usuarios extraer y trabajar con los datos como se demostró en la sección Cómo hacerlo.

## Ver también
- [Repositorio de GitHub de Gumbo-parser](https://github.com/google/gumbo-parser)
- [Lista de bibliotecas de análisis de HTML](https://en.cppreference.com/w/c/experimental/dynamic)
- [Interoperabilidad de C++ y Python](https://docs.python.org/3/extending/embedding.html)
