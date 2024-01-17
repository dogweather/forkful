---
title:                "Enviando una solicitud http"
html_title:           "C#: Enviando una solicitud http"
simple_title:         "Enviando una solicitud http"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/sending-an-http-request.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Enviar una solicitud HTTP es una forma de comunicarse con servidores web para obtener datos o realizar acciones específicas. Los programadores a menudo envían solicitudes HTTP para crear, leer, actualizar o eliminar información en una base de datos o en una aplicación web.

## Cómo:

```C#
// Ejemplo de envío de solicitud GET con la clase HttpClient.

using System;
using System.Net.Http;

namespace HttpExample
{
    class Program
    {
        static async Task Main(string[] args)
        {
            HttpClient client = new HttpClient();
            HttpResponseMessage response = await client.GetAsync("https://www.ejemplo.com/api/user/1");
            response.EnsureSuccessStatusCode();
            string responseBody = await response.Content.ReadAsStringAsync();
            Console.WriteLine(responseBody);
        }
    }
}
```
Resultado:
```
{"name": "Juan", "age": 25, "city": "Madrid"}
```

## Inmersión Profunda:

Históricamente, las solicitudes HTTP se usaban principalmente para acceder a documentos HTML en la web. Sin embargo, con el auge de las aplicaciones web, ahora también se utilizan para realizar acciones y obtener datos de una API. Algunas alternativas a las solicitudes HTTP incluyen el protocolo FTP para transferir archivos y el protocolo SMTP para enviar correos electrónicos. Para implementar una solicitud HTTP, se pueden utilizar diferentes librerías y herramientas, como la clase HttpClient en .NET o la librería Requests en Python.

## Vea también:

- Documentación oficial sobre solicitudes HTTP en C#: https://docs.microsoft.com/es-es/dotnet/api/system.net.http.httpclient?view=net-5.0
- Tutorial sobre cómo hacer solicitudes HTTP en C#: https://www.freecodecamp.org/news/using-httpclient-to-consume-webapi-in-net/