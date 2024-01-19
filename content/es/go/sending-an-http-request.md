---
title:                "Enviando una solicitud http"
html_title:           "Bash: Enviando una solicitud http"
simple_title:         "Enviando una solicitud http"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/sending-an-http-request.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?

Enviar una solicitud HTTP implica comunicarse con un servidor web. Los programadores lo hacen para interactuar con servicios web, descargar archivo, postear datos y más.

## Cómo hacerlo:

A continuación, un sencillo código que muestra cómo hacer una solicitud GET en Go.

```Go
package main

import (
	"io/ioutil"
	"log"
	"net/http"
)

func main() {
	response, err := http.Get("http://www.google.com/")
	if err != nil {
		log.Fatal(err)
	}
	defer response.Body.Close()

	body, err := ioutil.ReadAll(response.Body)
	if err != nil {
		log.Fatal(err)
	}

	log.Println(string(body))
}
```

Este script hará una solicitud GET a google.com y luego imprimirá la respuesta.

## Análisis Profundo

Go fue diseñado para facilitar la programación concurrente, por lo que enviar solicitudes HTTP es bastante sencillo. Sin embargo, en los primeros días de Go (antes de la versión 1.0), la situación era diferente y los programadores necesitaban usar paquetes adicionales.

Hay múltiples maneras de realizar solicitudes HTTP en Go más allá del método básico. Puedes usar el paquete `net/http/httputil` para simplificar la solicitud, el paquete `net/http/httptrace` permite rastrear los eventos de la solicitud HTTP y puedes manipular a bajo nivel las solicitudes HTTP con el paquete `net/http`.

## Ver También

Aquí tienes algunos enlaces que ofrecen más información y tutoriales detallados sobre cómo enviar solicitudes HTTP con Go:

- [Go por Ejemplo: Solicitudes HTTP](https://gobyexample.com/http-requests)
- [Paquete net/http](https://golang.org/pkg/net/http/)
- [Hacer solicitudes HTTP en Go](https://medium.com/rungo/making-external-http-requests-in-go-eb4c015f8839)