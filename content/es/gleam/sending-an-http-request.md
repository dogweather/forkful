---
title:                "Enviando una solicitud HTTP"
html_title:           "Gleam: Enviando una solicitud HTTP"
simple_title:         "Enviando una solicitud HTTP"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/sending-an-http-request.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Enviar una solicitud HTTP es un proceso mediante el cual se envían datos desde un programa informático a un servidor web. Los programadores lo hacen para comunicarse con otros sistemas o servicios externos, como API's o bases de datos, y así poder obtener o enviar información.

## Cómo:
Ejemplo de código en ```Gleam``` para enviar una solicitud GET a una API:

```
let resultado = Muestra
                .Servicio
                .Obtener(url)
                .expect("Error al realizar solicitud HTTP")

let texto = Http
            .Respuesta
            .Contenido(resultado)
            .expect("Error al obtener contenido de la respuesta")

```
El resultado esperado sería el contenido de la respuesta en formato de texto, que se puede utilizar en el programa para realizar operaciones adicionales.

## Inmersión profunda:
En la historia de la programación, el envío de solicitudes HTTP era una tarea mucho más compleja y manual. Gracias al avance de los lenguajes de programación y las librerías, hoy en día podemos realizar esta tarea de manera simple y eficiente.

Alternativas a ```Gleam``` para enviar solicitudes HTTP incluyen librerías en otros lenguajes como Python o JavaScript, o utilizando programas externos como cURL.

Las implementaciones internas de la función de enviar solicitud HTTP pueden variar dependiendo del lenguaje y la librería utilizada, pero en general, se utilizan protocolos de red como TCP y UDP para establecer conexión con el servidor y enviar y recibir los datos.

## Ver también:
- Documentación oficial de ```Gleam``` (https://gleam.run)
- Comparación de herramientas para realizar solicitudes HTTP (https://www.toptal.com/developers/http-and-rest/introduction-to-http-rest-and-apis)
- Tutorial para realizar solicitudes HTTP en diferentes lenguajes (https://www.tutorialspoint.com/http/http_requests.htm)