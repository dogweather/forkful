---
title:                "Descargando una página web"
html_title:           "Arduino: Descargando una página web"
simple_title:         "Descargando una página web"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## ¿Qué & Por Qué?
Descargar una página web significa obtener su código HTML para usarlo localmente. Los programadores hacen esto para analizar la estructura de la página, recolectar datos y probar funcionalidades del sitio.

## Cómo:
Ahora, veamos cómo descargar una página web con Go:

```Go
package main

import (
	"io/ioutil"
	"net/http"
	"os"
)

func main() {
	resp, err := http.Get("http://example.com")
	if err != nil {
		panic(err)
	}
	defer resp.Body.Close()

	body, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		panic(err)
	}

	f, err := os.Create("example.html")
	if err != nil {
		panic(err)
	}

	defer f.Close()

	_, err = f.Write(body)
	if err != nil {
		panic(err)
	}
}
```
Cuando ejecutas este código, descargará el HTML de "example.com" y lo guardará en un archivo llamado "example.html". 

## Inmersión Profunda
Descargar páginas web no es un concepto novedoso. Ha sido parte integral de la web desde sus inicios, habiendo múltiples formas de hacerlo. En lenguajes más antiguos como Perl o PHP, hay muchas funciones incorporadas para facilitar este proceso. 

En Go, la biblioteca net/http ofrece una interfaz bastante sencilla para realizar estos trabajos. Aunque existen otras alternativas como `http.NewRequest` y `DefaultClient.Do`, `http.Get` es suficiente para tareas simples y es lo que hemos utilizado en el ejemplo.

Implementar la descarga de una página web con Go implica trabajar con interfaces IO. Cuando se ejecuta `http.Get`, se devuelve un `*Response`, que contiene un `Body` de tipo `ReadCloser`, una interfaz que presenta métodos para lectura y cierre. Sin embargo, necesita convertirse en un `[]byte` para grabarlo en un archivo, lo cual realizamos con `ioutil.ReadAll`.

## Ver Tambien
1. Documentación oficial de Go para HTTP: https://golang.org/pkg/net/http/
2. Un buen recurso sobre interfaces IO en Go: https://go.dev/blog/io2013
3. Librería `goquery`: https://github.com/PuerkitoBio/goquery, que es útil para analizar el HTML descargado.