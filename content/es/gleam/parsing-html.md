---
title:                "Gleam: Analizando html"
simple_title:         "Analizando html"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/parsing-html.md"
---

{{< edit_this_page >}}

## ¿Por qué deberías aprender a analizar HTML con Gleam?

Si eres un desarrollador web, es esencial saber cómo analizar y manipular HTML. Además, Gleam es un lenguaje de programación moderno y funcional que ofrece una forma elegante y sencilla de manejar datos estructurados, como el HTML.

## Cómo hacerlo: Ejemplos de codificación

Para analizar HTML en Gleam, necesitamos usar una biblioteca externa llamada "gleam-html". Podemos instalar esta biblioteca usando el administrador de paquetes de Gleam:

```Gleam
em: install gleam-html
```

Una vez que la biblioteca esté instalada, podemos importarla en nuestro código y usarla para analizar el HTML:

```Gleam
import gleam/html as html

let html_string = "<h1>Hello world</h1>"

let result = html.parse(html_string)

// Output: Ok([Html.heading([Html.text('Hello world')])])
```

En el ejemplo anterior, estamos analizando una cadena de HTML y obteniendo una estructura de datos en formato Gleam que podemos usar en nuestro código.

## Profundizando en la analización de HTML

Además de parsear HTML, la biblioteca "gleam-html" también nos permite construir código HTML de forma programática y validar la estructura de HTML. Además, podemos realizar operaciones más complejas, como recorrer y manipular el árbol de HTML.

También es importante tener en cuenta que la biblioteca "gleam-html" es compatible con el estándar HTML5 y maneja de forma segura las etiquetas y atributos inválidos o mal formateados.

Ahora que tienes una idea de cómo analizar HTML con Gleam, ¡puedes comenzar a utilizarlo en tus proyectos web para un código más limpio y sencillo!

## Ver también

- Documentación oficial de "gleam-html": https://hexdocs.pm/gleam_html/html-module.html
- Ejemplos de uso de "gleam-html": https://github.com/gleam-lang/gleam-html/tree/master/examples
- Tutorial de Gleam: https://gleam.run/book/getting-started.html#toc_1