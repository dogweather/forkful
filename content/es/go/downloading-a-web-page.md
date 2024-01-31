---
title:                "Descargando una página web"
date:                  2024-01-20T17:44:16.095990-07:00
model:                 gpt-4-1106-preview
simple_title:         "Descargando una página web"

category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Descargar una página web significa traerte el contenido de una URL a tu programa. Lo hacemos para automatizar tareas como probar disponibilidad de sitios, analizar datos o gestionar contenido.

## Cómo hacerlo:
```Go
package main

import (
	"fmt"
	"io"
	"net/http"
	"os"
)

func main() {
	resp, err := http.Get("http://example.com")
	if err != nil {
		fmt.Fprintf(os.Stderr, "hubo un error: %v\n", err)
		os.Exit(1)
	}
	defer resp.Body.Close()

	io.Copy(os.Stdout, resp.Body)
}
```
Salida de muestra:
```
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
...
</html>
```
Recordá manejar errores y cerrar el cuerpo de respuestas para evitar fugas de recursos.

## Inmersión Profunda
La facultad de descargar páginas web viene desde los primeros días de la web. Alternativas como `curl` y `wget` son comunes en la línea de comandos, pero Go ofrece una forma programática poderosa con `http`. Implementaciones previas podrían haber usado librerías como `net/http/httputil`, pero `http.Get` simplifica el proceso. bajo el capó, Go gestiona las conexiones TCP, los protocolos HTTP, y puede trabajar con HTTPS también.

El manejo de errores es crucial: la red es impredecible, y tu aplicación necesita saber qué hacer cuando algo no funciona como se espera. Además, manipular el resultado puede implicar desde simplemente guardarlo hasta analizar el HTML, para lo cual podrías necesitar paquetes como `goquery`.

Por último, si estás creando una aplicación que descarga muchas páginas o accede con alta frecuencia, es importante ser respetuoso con los servidores y seguir prácticas como la limitación de tasa y el respeto a `robots.txt`.

## Ver También
- Documentación oficial de Go en `net/http`: https://pkg.go.dev/net/http
- Paquete `goquery` para manipular HTML en Go: https://pkg.go.dev/github.com/PuerkitoBio/goquery
- Prácticas recomendadas para web scraping: https://developer.mozilla.org/en-US/docs/Web/HTTP/CORS
