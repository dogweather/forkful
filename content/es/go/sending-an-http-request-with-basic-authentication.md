---
title:                "Enviando una solicitud http con autenticación básica"
date:                  2024-01-20T18:01:50.796734-07:00
model:                 gpt-4-1106-preview
simple_title:         "Enviando una solicitud http con autenticación básica"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Qué y por qué?
Enviar una solicitud HTTP con autenticación básica implica incluir credenciales de usuario y contraseña codificadas en base64 en el encabezado de la solicitud. Los programadores lo hacen para acceder a recursos protegidos en la web de forma segura y sencilla.

## Cómo hacerlo:
```Go
package main

import (
    "encoding/base64"
    "fmt"
    "net/http"
)

func main() {
    client := &http.Client{}
    req, err := http.NewRequest("GET", "http://tuapi.com/datos", nil)
    if err != nil {
        panic(err)
    }
    
    usuario := "miUsuario"
    contraseña := "miContraseña"
    credenciales := base64.StdEncoding.EncodeToString([]byte(usuario + ":" + contraseña))
    req.Header.Set("Authorization", "Basic " + credenciales)
    
    resp, err := client.Do(req)
    if err != nil {
        panic(err)
    }
    defer resp.Body.Close()
	
    fmt.Println("Estado de la respuesta:", resp.Status)
}
```
Salida esperada será algo como:
```
Estado de la respuesta: 200 OK
```
si la autenticación fue exitosa.

## Análisis detallado
La autenticación básica HTTP es un método antiguo, parte del protocolo desde el principio. Aunque hay métodos más seguros como OAuth, la autenticidad básica sigue siendo útil por su simplicidad. Básicamente, codifica el usuario y la contraseña con base64 y los coloca en el encabezado de la solicitud. Pero atención: como base64 es reversible, usar HTTPS es fundamental para evitar que las credenciales sean interceptadas en texto plano.

Alternativas modernas incluyen tokens JWT (Json Web Tokens) o la mencionada OAuth, que proporcionan mecanismos más sofisticados y seguros, especialmente en aplicaciones con gran cantidad de usuarios y datos sensibles.

## Ver también
- [Documentación oficial de Go para net/http](https://pkg.go.dev/net/http)
- [RFC 7617, 'The 'Basic' HTTP Authentication Scheme'](https://tools.ietf.org/html/rfc7617)
- [Tutorial sobre autenticación HTTP básica](https://developer.mozilla.org/es/docs/Web/HTTP/Authentication)
- [Comparación de métodos de autenticación HTTP](https://swagger.io/docs/specification/authentication/basic-authentication/)
