---
title:                "Enviando una solicitud http"
html_title:           "Bash: Enviando una solicitud http"
simple_title:         "Enviando una solicitud http"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/sending-an-http-request.md"
---

{{< edit_this_page >}}

---

## ¿Qué & Por qué?

Enviar una solicitud HTTP es establecer una comunicación entre un cliente y un servidor en la web. Los programadores la hacen para recoger, crear, actualizar o eliminar datos de un servidor web.

---

## Cómo hacerlo

Para enviar una solicitud HTTP, utilizamos la clase HttpClient que encontráis en el espacio de nombres System.Net.Http.

Aquí unos ejemplos básicos de cómo hacerlo en C#:

```C#
using System;
using System.Net.Http;
using System.Threading.Tasks;

class Program
{
    static readonly HttpClient client = new HttpClient();

    static async Task Main()
    {
        try
        {
            string responseBody = await client.GetStringAsync("https://api.github.com");

            Console.WriteLine(responseBody);
        }
        catch(HttpRequestException e)
        {
            Console.WriteLine("\nExcepción detectada!");
            Console.WriteLine("Mensaje : {0} ", e.Message);
        }
    }
}
```

Cuando ejecutes este programa, deberías ver una respuesta del servidor en la consola.

---

## En profundidad

Enviar una solicitud HTTP es una forma fundamental de interactuar con los recursos de la web, una práctica que se encuentra en los cimientos de Internet como lo conocemos. Existen alternativas como las solicitudes FTP o SMTP, pero las solicitudes HTTP son la norma para la mayoría de las aplicaciones web.

En cuanto a los detalles de implementación, la clase HttpClient maneja todas las peticiones y respuestas HTTP de manera eficiente. Es reutilizable y soporta varias solicitudes concurrentes. Puede manejar redirecciones automáticas, enviar solicitudes JSON, y mucho más.

Además, HttpClient es una clase de alto nivel que encapsula las funcionalidades de las clases de más bajo nivel como HttpWebRequest y HttpWebResponse.

---

## Ver también

- [Documentación oficial de HttpClient](https://docs.microsoft.com/es-es/dotnet/api/system.net.http.httpclient?view=net-5.0)
- [Más información sobre solicitudes HTTP](https://developer.mozilla.org/es/docs/Web/HTTP/Methods)