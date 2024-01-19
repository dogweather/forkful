---
title:                "Enviando una solicitud http con autenticación básica"
html_title:           "Arduino: Enviando una solicitud http con autenticación básica"
simple_title:         "Enviando una solicitud http con autenticación básica"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Enviar una solicitud HTTP con autenticación básica es un proceso de envío de credenciales (username y password) en el encabezado de una solicitud HTTP. Los programadores lo hacen para obtener acceso a recursos protegidos o realizar funciones específicas en un servidor.

## ¿Cómo hacerlo?
Aquí está el código de ejemplo en C#, que utiliza la librería `HttpClient` para enviar la solicitud HTTP con autenticación básica.

```C#
using System;
using System.Net.Http;
using System.Text;
using System.Threading.Tasks;

class Program
{
    static async Task Main(string[] args)
    {
        var httpClient = new HttpClient();
        var byteArray = Encoding.ASCII.GetBytes("username:password");
        httpClient.DefaultRequestHeaders.Authorization = new System.Net.Http.Headers.AuthenticationHeaderValue("Basic", Convert.ToBase64String(byteArray));

        var result = await httpClient.GetAsync("http://yourwebsite.com/resource");
        Console.WriteLine(result.StatusCode);
    }
}
```

Cuando ejecutes este código, esperarías ver el estado HTTP del servicio web en la consola.

## Inmersión profunda
La autenticación básica HTTP es una técnica antigua, su implementación es simple pero no es segura para transmitir información sensible ya que las credenciales se transmiten en texto plano (aunque codificado en base64). Hoy en día, se prefiere `Bearer token authentication`.

Un método alternativo para esto en C# es utilizar la biblioteca `HttpClientHandler`. 

Asegúrate de usar HTTPS en lugar de HTTP para garantizar que el contenido de las solicitudes se transmite de manera segura.

## Ver también
- Documentación de Microsoft sobre HttpClient: https://docs.microsoft.com/es-es/dotnet/api/system.net.http.httpclient
- Guía de autenticación básica HTTP: https://developer.mozilla.org/es/docs/Web/HTTP/Authentication
- Autenticación Oauth y Bearer tokens: https://auth0.com/learn/token-based-authentication-made-easy