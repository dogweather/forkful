---
title:                "Enviando una solicitud http con autenticación básica"
html_title:           "Kotlin: Enviando una solicitud http con autenticación básica"
simple_title:         "Enviando una solicitud http con autenticación básica"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Enviar una solicitud HTTP con autenticación básica es un proceso en el que se envía una solicitud a un servidor web con credenciales básicas de inicio de sesión para acceder a cierta información o recursos protegidos. Los programadores lo hacen para garantizar que solo los usuarios autorizados puedan acceder a ciertos datos o funcionalidades en una aplicación web.

## Cómo:

``` Kotlin
// Primero, importamos las bibliotecas necesarias
import java.net.HttpURLConnection
import java.net.URL

// Definimos la URL del servidor y las credenciales de inicio de sesión
var url = URL("https://ejemplo.com")
var username = "usuario"
var password = "contraseña"

// Luego, creamos una conexión HTTP y establecemos la propiedad de autenticación básica
var connection = url.openConnection() as HttpURLConnection
var auth = "Basic " + Base64.getEncoder().encodeToString("$username:$password".toByteArray())
connection.setRequestProperty("Authorization", auth)

// Finalmente, enviamos la solicitud y obtenemos la respuesta
var responseCode = connection.responseCode
println("Response Code: $responseCode")

var inputStream = connection.inputStream
var inputAsString = inputStream.bufferedReader().use { it.readText() }
println("Response Body: \n$inputAsString")

```

La consola imprimirá algo similar a:

```
Response Code: 200
Response Body: 
{"nombre": "Juan", "apellido": "Pérez"}
```

## Profundizando

La autenticación básica es uno de los métodos más antiguos y simples utilizados para proteger recursos en una aplicación web. Fue introducida en el protocolo HTTP en los años 90 y es compatible con la mayoría de los servidores y navegadores web. Sin embargo, tiene algunas debilidades de seguridad ya que las credenciales se envían en texto claro y pueden ser interceptadas. 

Otra alternativa para autenticar solicitudes HTTP es mediante el uso de tokens de acceso, que son generados por un servidor de autorización y son más seguros que las credenciales básicas. Sin embargo, es importante tener en cuenta que la implementación correcta de estos métodos de autenticación depende del tipo de aplicación y sus requisitos de seguridad.

## Ver También

- [HTTP Basic Authentication en MDN](https://developer.mozilla.org/es/docs/Web/HTTP/Authentication#basic_authentication_scheme)
- [Kotlin HTTP Requests Tutorial](https://www.tutorialkart.com/kotlin/kotlin-http-request-async-get-post-using-fuel-library/)
- [Securing Your REST API With Basic Authentication](https://dzone.com/articles/securing-your-rest-api-with-basic-authentication)