---
title:                "Analizando html"
html_title:           "Gleam: Analizando html"
simple_title:         "Analizando html"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/parsing-html.md"
---

{{< edit_this_page >}}

¡Hola, amigos! ¿Están buscando una forma fácil de analizar y manipular código HTML en su programa? Si es así, ¡entonces han llegado al lugar correcto! En este artículo, les mostraré cómo utilizar Gleam para hacer el parsing de HTML de una manera sencilla y eficiente.

## ¿Por qué?

Es posible que se pregunten por qué querrían realizar el parsing de HTML en primer lugar. Bueno, existen muchas razones, pero una de las más comunes es la de extraer información específica de una página web. Por ejemplo, si quieren obtener el título o el contenido de un artículo de un sitio de noticias, el parsing de HTML es lo que necesitan para hacerlo de manera automática.

## Cómo hacerlo

La sintaxis básica de un programa Gleam es la siguiente:

```Gleam
import http
import gleam/html

let html = http.get("https://www.example.com")
let dom = html |> html.parse
```

En este ejemplo, estamos importando los módulos necesarios para hacer la petición a una página web y para analizar su contenido HTML. Luego, utilizamos la función "get" del módulo http para obtener el contenido de la página y lo pasamos a la función "parse" del módulo gleam/html para obtener un documento DOM (modelo de objeto de documento).

Para extraer información específica del DOM, podemos utilizar el patrón de coincidencia de Gleam. Por ejemplo, si queremos obtener todos los enlaces de una página, podemos hacer lo siguiente:

```Gleam
let links =
  dom
  |> html.selectAll("a")
  |> List.map(html.getAttribute("href"))
  |> List.filter(Option.isSome)
```

En este caso, estamos filtrando solo los enlaces que tienen un atributo "href" y mapeándolos a una lista de cadenas de texto.

## Deep Dive

Si desean profundizar en el tema, Gleam también tiene una función "selectOne" que les permite seleccionar un solo elemento del DOM. Además, pueden utilizar el módulo gleam/regex para hacer coincidir patrones en el contenido HTML y extraer información de manera más específica.

¡Eso es todo por hoy, amigos! Espero que hayan encontrado útil este artículo sobre el parsing de HTML con Gleam. ¡Pero esto es solo el comienzo! Pueden seguir investigando y descubrir más formas de utilizar Gleam para analizar y manipular HTML en sus proyectos.

## Ver también

- Documentación oficial de Gleam: https://gleam.run/
- Módulo HTTP de Gleam: https://gleam.run/modules/http.html
- Módulo HTML de Gleam: https://gleam.run/modules/html.html
- Módulo Regex de Gleam: https://gleam.run/modules/regex.html