---
title:                "Enviando una solicitud http"
html_title:           "Bash: Enviando una solicitud http"
simple_title:         "Enviando una solicitud http"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/sending-an-http-request.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Enviar una solicitud HTTP es un proceso de comunicación entre el cliente (Tu aplicación) y el servidor utilizando el protocolo HTTP. Los programadores lo hacen para interactuar con APIs, recuperar información, enviar datos y mucho más.

## ¿Cómo se hace?

El envío de solicitudes HTTP en Kotlin es sencillo gracias al uso de bibliotecas como Ktor. Veamos un ejemplo:

```Kotlin
val client = HttpClient()
val httpResponse: HttpResponse = client.get("http://sample.com")
val responseData: String = httpResponse.receive()

println(responseData)
```

En este caso, estamos usando el método `get` para recuperar datos de `sample.com` y luego los mostramos en consola.

## Inmersión Profunda

Históricamente, las solicitudes HTTP se han utilizado como la forma principal de comunicación entre los clientes y los servidores desde el surgimiento de la web. Sin embargo, existen alternativas como el protocolo WebSocket para conexiones en tiempo real y GraphQL para consultar APIs.

En cuanto a los detalles de implementación, Kotlin maneja las solicitudes HTTP de manera eficaz al usar bibliotecas como Ktor y Fuel. Estas bibliotecas incorporan características modernas como la programación asíncrona y la programación reactiva, lo que facilita enormemente la tarea del programador.

## Ver También

Aquí hay algunas fuentes adicionales que pueden ser de tu interés:
* Documentación Ktor: https://ktor.io/clients/http-client/engines.html
* Documentación Fuel: https://fuel.github.io/
* API de Java para HTTP/2 y WebSocket: https://openjdk.java.net/jeps/321
* Documentación GraphQL: https://graphql.org/learn/