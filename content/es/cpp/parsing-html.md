---
title:                "Analizando html"
html_title:           "C++: Analizando html"
simple_title:         "Analizando html"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/parsing-html.md"
---

{{< edit_this_page >}}

## Por qué

¿Te has preguntado alguna vez cómo los navegadores web son capaces de mostrar páginas web correctamente? La respuesta está en el lenguaje HTML, utilizado para crear y estructurar el contenido de una página web. En este artículo, descubriremos por qué es necesario parsear HTML cuando se trabaja con data en C++, y cómo puedes hacerlo tú mismo.

## Cómo hacerlo

Para parsear HTML en C++, utilizaremos la biblioteca "libxml2". Esta biblioteca es completa y fácil de usar, y te permite leer y manipular archivos HTML con facilidad. A continuación, se muestra un ejemplo sencillo de cómo parsear un archivo HTML utilizando libxml2:

```C++
#include <libxml/tree.h>
#include <libxml/HTMLparser.h>
#include <iostream>

int main() {
    // Crear un parser de HTML
    htmlParserCtxtPtr parser = htmlCreatePushParserCtxt(NULL, NULL, NULL, 0, NULL, 0);

    // Abrir un archivo HTML y leerlo línea por línea
    FILE* archivo = fopen("página.html", "r");
    while (!feof(archivo)) {
        // Leer una línea del archivo y parsearla
        char buffer[1024];
        fgets(buffer, 1024, archivo);
        htmlParseChunk(parser, buffer, strlen(buffer), 0);
    }

    // Obtener el árbol de nodos del documento HTML
    xmlDocPtr documento = parser->myDoc;
    xmlNodePtr raiz = xmlDocGetRootElement(documento);

    // Recorrer los nodos e imprimir sus nombres
    xmlNodePtr nodo = raiz;
    while (nodo != NULL) {
        std::cout << nodo->name << std::endl;
        nodo = nodo->next;
    }

    // Cerrar el parser y el archivo
    htmlFreeParserCtxt(parser);
    fclose(archivo);
    
    return 0;
}
```

En este ejemplo, utilizamos la función `htmlParseChunk` para parsear cada línea del archivo HTML y obtener un árbol de nodos del documento. Luego, utilizamos la función `xmlDocGetRootElement` para obtener el nodo raíz del documento, y recorremos los demás nodos utilizando la propiedad `next`.

El resultado de este código sería la impresión de los nombres de todos los nodos del documento HTML.

## Profundizando

El proceso de parsear HTML en realidad implica mucho más que simplemente obtener una estructura de nodos del documento. El parser también se encarga de validar la estructura del documento y convertir el contenido de los nodos a un formato legible para la computadora.

Además, existen diferentes tipos de nodos que pueden aparecer en un documento HTML, como etiquetas, texto, comentarios y atributos. Cada uno de estos tipos de nodos tiene sus propios métodos y propiedades que pueden ser accedidos a través del árbol de nodos.

No es necesario conocer todos los detalles y especificaciones del lenguaje HTML para poder parsear un archivo en C++, pero es importante tener una comprensión básica de cómo funciona el lenguaje y su estructura para poder manipular correctamente los nodos.

## Ver también

* [Documentación de la biblioteca "libxml2"](http://www.xmlsoft.org/html/)
* [Tutorial de "libxml2" para parsear HTML en C++](https://www.xml.com/pub/a/2000/10/18/libxml/index.html)
* [Especificaciones del lenguaje HTML](https://www.html5rocks.com/en/tutorials/internals/howbrowserswork/#Main_flow_of_HTML_receiver)