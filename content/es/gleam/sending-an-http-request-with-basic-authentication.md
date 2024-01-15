---
title:                "Enviando una solicitud http con autenticación básica"
html_title:           "Gleam: Enviando una solicitud http con autenticación básica"
simple_title:         "Enviando una solicitud http con autenticación básica"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## ¿Por qué hacer una solicitud HTTP con autenticación básica?

En la era actual de la tecnología, es común que las aplicaciones web se comuniquen entre sí a través de solicitudes HTTP. Sin embargo, para acceder a ciertos recursos protegidos, como información confidencial o funciones importantes de una aplicación, es necesario añadir una capa adicional de seguridad. La autenticación básica es una forma sencilla pero efectiva de verificar la identidad del usuario que realiza la solicitud, lo que la convierte en una práctica clave en el desarrollo de aplicaciones web seguras.

## Cómo hacer una solicitud HTTP con autenticación básica en Gleam

Para enviar una solicitud HTTP con autenticación básica en Gleam, se necesita utilizar la librería HTTP nativa "glix" y la función `Http.request_with_basic_auth()`. A continuación, se muestra un ejemplo de código que realiza una solicitud POST a una API con autenticación básica y devuelve la respuesta JSON recibida:

```gleam
import glix

pub fn send_post_request_with_basic_auth() {
  let url = "https://example.com/api/resource";
  let body = "{\"username\": \"user123\", \"password\": \"secretpass\"}";
  
  let auth = glix.Http.BasicAuth("user123", "secretpass");
  
  let result = glix.Http.request_with_basic_auth(
    glix.Http.method_post(), 
    url, 
    glix.Http.Body.text(body), 
    auth
  );
  
  case result {
    Ok(response) -> println("Response received: {}", response.body)
    Err(error) -> println("Error encountered: {}", error)
  }
}
```

La salida de este código mostrará la respuesta recibida de la API, que podría ser un objeto JSON con la información solicitada.

## Profundizando en las solicitudes HTTP con autenticación básica

Para entender mejor cómo funciona la autenticación básica en una solicitud HTTP, es importante tener en cuenta que cada solicitud incluye un header de encabezado llamado "Authorization". En una solicitud con autenticación básica, el valor de este header consiste en la palabra "Basic" seguida por un espacio, y luego el nombre de usuario y contraseña codificados en base64. Esto es lo que permite al servidor verificar la identidad del usuario.

Es importante tener en cuenta que aunque la autenticación básica es una forma rápida de añadir seguridad a una solicitud HTTP, no es considerada la más segura. Toda la información viaja en texto plano, lo que hace más fácil que un atacante pueda interceptar y descifrar la contraseña. En caso de necesitar un mayor nivel de seguridad, se recomienda utilizar otras formas de autenticación, como OAuth2.

## Vea También

- [Documentación de "glix" en Gleam](https://gleam.run/libraries-and-bindings/http.html#request-with-basic-auth) 
- [Ejemplo de solicitud HTTP con autenticación básica en Gleam](https://github.com/gleam-lang/examples/blob/master/http_basic_auth/src/basics.gleam)

Gracias por leer este artículo y esperamos que te haya sido útil para aprender sobre cómo realizar solicitudes HTTP con autenticación básica en Gleam. Si te interesa seguir aprendiendo sobre desarrollo web en Gleam, asegúrate de revisar nuestra documentación y ejemplos. ¡Hasta la próxima!