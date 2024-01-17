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

¡Hola programadores! ¿Alguna vez te has preguntado cómo descargar una página web utilizando Go? Bueno, ¡estás de suerte! En este artículo, te mostraremos cómo hacerlo de manera simple y concisa.

## ¿Qué & Por qué?
Descargar una página web es simplemente obtener el código HTML de una página web determinada. Los programadores lo hacen porque necesitan acceder y analizar el contenido de ese sitio web para diversos fines, como extraer datos o crear herramientas de automatización.

## Cómo:
En Go, podemos utilizar la biblioteca "net/http" para realizar una solicitud GET a una URL específica y obtener el cuerpo de respuesta, que es el código HTML de la página web. Eso suena complicado, pero en realidad es bastante simple. Echa un vistazo al siguiente código:

```
package main

import (
    "fmt"
    "net/http"
)

func main() {
    // hacemos una solicitud GET a la página web de Google
    res, err := http.Get("https://www.google.com/")

    // verificamos si hay algún error en la solicitud
    if err != nil {
        fmt.Println("¡Hubo un error!")
    }

    // imprimimos el cuerpo de la respuesta
    fmt.Println(res.Body)
}
```

El output debería ser algo como esto:
```
<!doctype html>
...
<!-- el resto del código HTML de la pagina de Google -->
```

## Deep Dive:
Ahora, echemos un vistazo más profundo a la biblioteca "net/http". Esta biblioteca proporciona un cliente HTTP interno, lo que significa que no es necesario importar otras bibliotecas para realizar solicitudes HTTP. Además, la biblioteca viene con una variedad de funciones útiles para trabajar con solicitudes y respuestas HTTP.

Si prefieres utilizar una biblioteca de terceros, también puedes utilizar "goquery", que te permite analizar y manipular fácilmente el HTML de la respuesta de la solicitud. Otra opción es "gocolly", que es una biblioteca de web scraping en Go.

En cuanto a la implementación, Go utiliza una técnica llamada "goroutines" para manejar múltiples solicitudes HTTP al mismo tiempo, lo que le permite ser altamente eficiente en la realización de solicitudes web.

## Ver También:
- Documentación oficial de Go sobre la biblioteca "net/http": https://golang.org/pkg/net/http/
- Documentación oficial de "goquery": https://github.com/PuerkitoBio/goquery
- Documentación oficial de "gocolly": https://github.com/gocolly/colly