---
title:                "C: Analizando html"
simple_title:         "Analizando html"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/parsing-html.md"
---

{{< edit_this_page >}}

## Por qué

En esta ocasión, hablaremos de una tarea bastante común en el mundo de la programación: analizar y extraer información de páginas web en HTML. Esta es una habilidad que puede resultar muy útil en diversas situaciones, como en el desarrollo de aplicaciones web o en la recolección de datos para un proyecto de investigación. Aprender a hacer este proceso en lenguaje C te permitirá automatizar tareas y ahorrar tiempo en tus proyectos.

## Cómo hacerlo

Antes de empezar, es importante tener conocimiento básico de HTML y de cómo funciona una página web. También necesitaremos una biblioteca llamada "libxml" que nos facilitará el proceso de análisis de documentos HTML. Una vez que tengamos estas bases, podremos seguir los siguientes pasos:

##### Paso 1: Incluye la biblioteca

Primero, debemos incluir la biblioteca libxml en nuestro código C, utilizando la directiva `#include`. También, incluiremos la biblioteca `stdio` para poder imprimir los resultados en la consola.

##### Paso 2: Definir variables

A continuación, definiremos las variables que necesitaremos para almacenar el documento HTML, así como los datos que queremos extraer de él.

```
xmlDocPtr doc;
xmlNodePtr node;
char *url = "https://www.example.com";
```

En este caso, estamos definiendo un puntero al documento HTML, un puntero al nodo que queremos analizar y una cadena de caracteres con la URL del sitio web que queremos analizar.

##### Paso 3: Obtener el documento

Ahora procederemos a obtener el documento del sitio web indicado utilizando la función `xmlParseFile`.

```
doc = xmlParseFile(url);
```

##### Paso 4: Recorrer el documento

Con la función `xmlDocGetRootElement` obtendremos el nodo raíz del documento y con la función `xmlParseDocument` obtendremos el siguiente nodo del tipo especificado.

```
node = xmlDocGetRootElement(doc);
node = xmlParseDocument(node, "head");
```

De esta forma, podremos acceder a los datos que se encuentran en la sección "head" del documento HTML.

##### Paso 5: Imprimir los resultados

Por último, utilizando la función `printf` podremos imprimir los resultados en la consola. Por ejemplo, si queremos obtener el título de la página web, podemos utilizar la función `xmlGetProp` para obtener el valor del atributo "title" del nodo "title".

```
char *title = xmlGetProp(node, "title");
printf("El título del sitio web es: %s", title);
```

El resultado impreso en la consola sería: "El título del sitio web es: Example".

## Inmersión Profunda

Como has podido ver, el proceso de analizar documentos HTML en C no es muy complicado si tenemos los conocimientos básicos y utilizamos la biblioteca correcta. Sin embargo, es importante tener en cuenta que cada página web es diferente y puede tener una estructura HTML distinta, por lo que es necesario adaptar el código según sea necesario. Además, existen muchas otras funciones y técnicas que pueden ayudarnos a analizar con mayor profundidad y precisión los documentos HTML.

## Ver también

- [Documentación de la biblioteca libxml](http://www.xmlsoft.org/html/libxml-tree.html)
- [Tutorial de HTML en W3Schools](https://www.w3schools.com/html/)
- [Ejemplos de análisis de documentos HTML en C](https://www.programiz.com/c-programming/examples/parse-html)