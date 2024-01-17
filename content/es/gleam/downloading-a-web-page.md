---
title:                "Descargando una página web"
html_title:           "Gleam: Descargando una página web"
simple_title:         "Descargando una página web"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# ¿Qué es y por qué se hace?

Descargar una página web es obtener el código HTML de una página en internet. Los programadores realizan esto para acceder a la información de la página y utilizarla en sus propios programas.

# Cómo hacerlo:

```Gleam 
fn main() {
  let response = download("https://google.com");
  let text = response.text();
  println(text);
}
```

Este código descargará la página de Google y mostrará su contenido en la consola. Para personalizar lo que se descarga, se pueden utilizar diferentes funciones como ```headers``` para especificar encabezados o ```params``` para agregar parámetros a la URL.

# Profundizando:

Descargar páginas web es una práctica común para obtener datos de internet. Alternativas a Gleam para lograr esto incluyen librerías como ```reqwest``` o ```curl```. Gleam también ofrece la posibilidad de manejar respuestas en formatos como JSON para facilitar el procesamiento de datos.

# Ver también:

Para más información sobre cómo descargar páginas web con Gleam, puedes consultar la documentación oficial: https://gleam.run/documentation/working-with-http/ También puedes revisar diferentes ejemplos de código en el directorio de ejemplos de Gleam: https://github.com/gleam-lang/gleam/tree/master/examples