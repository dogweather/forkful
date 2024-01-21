---
title:                "Enviando una solicitud http con autenticación básica"
date:                  2024-01-20T18:01:31.319522-07:00
model:                 gpt-4-1106-preview
simple_title:         "Enviando una solicitud http con autenticación básica"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Enviar una solicitud HTTP con autenticación básica es mandar peticiones a un servidor que requiere usuario y contraseña. Los programadores lo hacen para interactuar con servicios web asegurados y proteger el acceso a datos.

## Cómo hacerlo:
```gleam
import gleam/http
import gleam/http/elli
import gleam/base64

fn basic_auth_header(username: String, password: String) -> http.Header {
  let credentials = username ++ ":" ++ password
  let encoded_credentials = base64.encode(credentials)
  http.header("Authorization", "Basic " ++ encoded_credentials)
}

pub fn send_request() {
  let header = basic_auth_header("mi_usuario", "mi_contraseña")
  let response = http.get("https://tu_servidor.com/datos", [header])

  case response {
    Ok(response) -> 
      io.println(response.body) // Muestra la respuesta del servidor
    Error(error) ->
      io.println(error) // Imprime el error en caso de haberlo
  }
}
```
**Salida de muestra:**
```
{status: 200, body: "Respuesta exitosa del servidor con datos autenticados."}
```
o si hay un error:
```
"Error de conexión al servidor o credenciales inválidas."
```

## Profundización
La autenticación básica es un método clásico de HTTP, incluido por primera vez en 1996 con el RFC 1945. Hay alternativas más seguras como tokens de portador (Bearer tokens) y OAuth, pero la autenticación básica aún se usa por su simplicidad. En Gleam, el proceso es directo: codificamos las credenciales en Base64 y las agregamos en la cabecera HTTP. Aunque es fácil, es vital usar HTTPS para evitar que las credenciales se expongan en texto claro.

## Ver también
- [Gleam HTTP Documentation](https://hexdocs.pm/gleam_http/)
- [Basic Authentication on MDN](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
- [RFC 7617 - The 'Basic' HTTP Authentication Scheme](https://tools.ietf.org/html/rfc7617)