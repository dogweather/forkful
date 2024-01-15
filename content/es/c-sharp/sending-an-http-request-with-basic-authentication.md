---
title:                "Enviar una solicitud http con autenticación básica"
html_title:           "C#: Enviar una solicitud http con autenticación básica"
simple_title:         "Enviar una solicitud http con autenticación básica"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Por qué

¿Alguna vez te has preguntado cómo los usuarios acceden a aplicaciones y sitios web protegidos por contraseña? Bueno, todo se debe a las solicitudes HTTP con autenticación básica.

## Cómo hacerlo

Para enviar una solicitud HTTP con autenticación básica en C#, sigue estos sencillos pasos:

1. Importa el espacio de nombres `System.Net`.
2. Crea un objeto de la clase `WebRequest` y asigna la URL a la que deseas enviar la solicitud.
3. Agrega las credenciales de autenticación al objeto de la solicitud mediante el método `WebRequest.Credentials`.
4. Llama al método `GetResponse()` en el objeto de la solicitud para obtener la respuesta del servidor.
5. Si la autenticación es exitosa, la respuesta del servidor será un código de estado HTTP 200 (OK) y podrás acceder al contenido de la respuesta mediante la propiedad `WebResponse.GetResponseStream()`.

Aquí hay un ejemplo de código que muestra cómo enviar una solicitud HTTP con autenticación básica y imprimir el contenido de la respuesta:

```C#
using System;
using System.Net;
class Program
{
    static void Main(string[] args)
    {
        string url = "https://api.ejemplo.com/informacion-protegida";
        WebRequest solicitud = WebRequest.Create(url);
        solicitud.Credentials = new NetworkCredential("nombre-usuario", "contraseña");
        WebResponse respuesta = solicitud.GetResponse();
        Console.WriteLine(respuesta.GetResponseStream());
    }
}
```

## Profundizando

Ahora que sabemos cómo enviar una solicitud HTTP con autenticación básica, exploremos un poco más en detalle cómo funciona todo esto. En términos sencillos, la autenticación básica en una solicitud HTTP implica agregar un encabezado de `Authorization` a la solicitud con las credenciales de acceso. Este encabezado incluye el nombre de usuario y la contraseña (en formato Base64) en un formato específico establecido por la especificación HTTP.

Es importante tener en cuenta que la autenticación básica es un método de autenticación muy básico y no es muy seguro, ya que las credenciales se envían en texto plano. Además, solo se utiliza para enviar solicitudes a través de canales seguros como HTTPS.

## Ver también

- [Documentación oficial de Microsoft sobre el espacio de nombres System.Net en C#](https://docs.microsoft.com/en-us/dotnet/api/system.net?view=net-5.0)
- [Especificación de autenticación básica HTTP en la IETF](https://tools.ietf.org/html/rfc7235#section-2.2)