---
title:                "C#: Enviando una solicitud http con autenticación básica"
simple_title:         "Enviando una solicitud http con autenticación básica"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Por qué

Enviar solicitudes HTTP con autenticación básica es una forma segura y eficiente de asegurar la comunicación entre un cliente y un servidor. Al utilizar este método de autenticación, se requiere que el cliente proporcione un nombre de usuario y una contraseña para acceder a recursos protegidos en el servidor.

## Cómo hacerlo

Para enviar una solicitud HTTP con autenticación básica en C#, primero necesitaremos establecer la conexión con el servidor utilizando la clase WebClient. Luego, utilizaremos la propiedad Credentials para proporcionar el nombre de usuario y la contraseña en la solicitud.

A continuación se muestra un ejemplo de código que muestra cómo enviar una solicitud GET con autenticación básica a una URL específica y recibir una respuesta:

```C#
// Crear una nueva instancia de WebClient
using (WebClient cliente = new WebClient())
{
    // Establecer las credenciales para la autenticación básica
    cliente.Credentials = new NetworkCredential("nombre_de_usuario", "contraseña");

    // Hacer una solicitud GET a la URL especificada y recibir la respuesta
    Console.WriteLine(cliente.DownloadString("http://www.ejemplo.com"));
}
```

La respuesta recibida se imprimirá en la consola, lo que permitirá al usuario verificar que la solicitud se haya realizado correctamente.

## Inmersión profunda

La autenticación básica utiliza el protocolo HTTP para transmitir las credenciales de usuario en texto plano. Esto significa que si alguien intercepta la solicitud, podría obtener fácilmente los nombres de usuario y contraseñas. Por esta razón, se recomienda utilizar la conexión HTTPS para una mayor seguridad en lugar de HTTP al enviar solicitudes con autenticación básica.

Además, si se está trabajando con una API, es posible que algunas solicitudes requieran una autenticación más compleja en lugar de una simple autenticación básica. En estos casos, es importante consultar la documentación correspondiente para asegurarse de enviar la solicitud correctamente.

## Ver también

- [WebClient Class - Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/api/system.net.webclient?view=net-5.0)
- [NetworkCredential Class - Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/api/system.net.networkcredential?view=net-5.0)
- [HTTP Authentication - Mozilla Developer Network](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)