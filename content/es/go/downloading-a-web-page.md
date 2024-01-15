---
title:                "Descargando una página web"
html_title:           "Go: Descargando una página web"
simple_title:         "Descargando una página web"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## ¿Por qué descargar una página web?

Descargar una página web puede ser útil para acceder a su contenido sin necesidad de una conexión a internet o para realizar web scraping, es decir, extraer datos de la página para su posterior análisis.

## Cómo hacerlo en Go

Para descargar una página web en Go, podemos utilizar la biblioteca estándar `http` y su método `Get`. A continuación, un ejemplo de código que descarga la página principal de Google y muestra su contenido en la consola:

```Go
resp, err := http.Get("https://www.google.com")
if err != nil {
    fmt.Println("Error al descargar la página:", err)
}
defer resp.Body.Close()

body, err := ioutil.ReadAll(resp.Body)
if err != nil {
    fmt.Println("Error al leer el cuerpo de la respuesta:", err)
}

fmt.Println(string(body))
```

El resultado de este código sería una gran cantidad de HTML, ya que es el formato en el que se muestra el contenido de una página web.

## Profundizando en la descarga de páginas web

El método `Get` de la biblioteca `http` también nos proporciona la respuesta completa del servidor, incluyendo el estado de la petición, los encabezados y el cuerpo de la respuesta. Además, podemos utilizar otras bibliotecas como`net/http" y "io/ioutil` para procesar el contenido de la respuesta de una manera más estructurada.

En el ejemplo anterior, utilizamos `ioutil.ReadAll` para leer todo el cuerpo de la respuesta y convertirlo en una cadena de texto legible. Sin embargo, también podríamos utilizar otras funciones como `ioutil.ReadAll` para obtener archivos específicos de la página o`html.Parse` de la biblioteca `"golang.org/x/net/html` para analizar el HTML y extraer información específica.

## Vea también

- [Documentación oficial de la biblioteca http en Go](https://golang.org/pkg/net/http/)
- [Ejemplos de web scraping en Go](https://github.com/gocolly/colly)