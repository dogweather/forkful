---
title:                "Enviando una solicitud http con autenticación básica"
html_title:           "Arduino: Enviando una solicitud http con autenticación básica"
simple_title:         "Enviando una solicitud http con autenticación básica"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

---

## ¿Qué y Por Qué?

La solicitud de HTTP con autenticación básica es enviar una solicitud a un servidor utilizando credenciales (usuario y contraseña) para obtener acceso. Hacemos esto para interactuar con la parte de API de un sitio que requiere autenticación, por ejemplo, la mayoría de los sitios web que tienen datos de usuario.

## ¿Cómo Se Hace?

Aquí te dejo un breve ejemplo de Go:

```Go
package main

import (
	"net/http"
)

func main() {
	req, err := http.NewRequest("GET", "http://example.com", nil)
	if err != nil {
	    panic(err)
	}
	req.SetBasicAuth("usuario", "contraseña")

	resp, err := http.DefaultClient.Do(req)
	if err != nil {
	    panic(err)
	}
	defer resp.Body.Close()
	// Hacer algo con resp.Body
}
```

Para probarlo, puedes reemplazar "http://example.com" con cualquier URL que requiera autenticación. Recuerda cambiar "usuario" y "contraseña" por tus credenciales reales.

## Inmersión Profunda

La autenticación básica es un método antiguo, sin embargo, todavía se utiliza en aplicaciones modernas. Aunque existen alternativas más seguras y complejas, como OAuth y autenticación basada en Tokens, la autenticación básica sigue siendo una opción popular por su simplicidad.

En Go, la librería http proporciona funciones para manejar las solicitudes HTTP y establecer la autenticación básica. La función `SetBasicAuth` agrega el encabezado 'Authorization' automáticamente que es necesario para la autenticación básica.

## Ver También

Puedes encontrar más información y detalles técnicos en los siguientes recursos:

- [Golang 'net/http' paquete de documentación](https://golang.org/pkg/net/http/)
- [Http y Web autenticación básica](https://developer.mozilla.org/es/docs/Web/HTTP/Authentication)
- [REST API autenticación básica en detalle](https://nordicapis.com/understanding-basic-and-bearer-http-authentication/)

---