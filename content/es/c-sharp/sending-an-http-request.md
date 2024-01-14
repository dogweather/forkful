---
title:                "C#: Enviando una solicitud http"
simple_title:         "Enviando una solicitud http"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/sending-an-http-request.md"
---

{{< edit_this_page >}}

## ¿Por qué enviar una solicitud HTTP?

Enviar una solicitud HTTP es una parte fundamental de cualquier programa o aplicación que se comunique con un servidor en internet. Esto permite obtener y enviar información, realizar acciones y actualizar datos de manera rápida y eficiente.

## Cómo enviar una solicitud HTTP

Para enviar una solicitud HTTP en C#, primero debemos importar el espacio de nombres `System.Net` y utilizar la clase `HttpClient` para crear un cliente HTTP. A continuación, podemos utilizar el método `GetAsync()` para especificar la dirección URL a la que deseamos enviar la solicitud y esperar una respuesta del servidor.

```C#
using System.Net;

HttpClient client = new HttpClient();
HttpResponseMessage response = await client.GetAsync("https://ejemplo.com");
Console.WriteLine(response.StatusCode);
```

En este ejemplo, utilizamos `GetAsync()` para enviar una solicitud `GET` al sitio web "ejemplo.com" y guardar la respuesta en una variable. Luego, imprimimos el código de estado de la respuesta utilizando `Console.WriteLine()`. 

Otra forma de enviar una solicitud HTTP es utilizando el método `PostAsync()`, que nos permite enviar datos al servidor junto con la solicitud. Por ejemplo, si queremos enviar un formulario con información de usuario, podemos hacerlo de la siguiente manera:

```C#
using System.Net;
using System.Collections.Generic;
using System.Collections.Specialized;

HttpClient client = new HttpClient();

var userData = new Dictionary<string, string>
{
    { "username", "ejemplo" },
    { "password", "123456" }
};
var content = new FormUrlEncodedContent(userData);
HttpResponseMessage response = await client.PostAsync("https://ejemplo.com/login", content);

Console.WriteLine(response.StatusCode);
```

En este caso, utilizamos `PostAsync()` para enviar una solicitud `POST` al sitio web "ejemplo.com/login" con los datos del usuario en un formato de formulario URL y obtenemos la respuesta del servidor. 

## Profundizando en el envío de solicitudes HTTP

Además de `GetAsync()` y `PostAsync()`, existen otros métodos como `PutAsync()`, `DeleteAsync()` y `PatchAsync()` para enviar diferentes tipos de solicitudes HTTP. También podemos agregar encabezados de solicitud personalizados utilizando la propiedad `Headers` de la clase `HttpClient` o utilizar una `HttpClientHandler` para manejar cookies y otros detalles de la sesión. 

Es importante tener en cuenta que al enviar solicitudes HTTP, es necesario manejar posibles errores de conexión o respuestas del servidor. Podemos hacerlo utilizando un bloque `try-catch` alrededor del código que envía la solicitud.

```C#
try
{
    HttpResponseMessage response = await client.GetAsync("https://ejemplo.com");
    response.EnsureSuccessStatusCode(); // se lanza una excepción si hay un error de conexión o un código de estado inesperado del servidor
    Console.WriteLine("Solicitud enviada con éxito");
}
catch (HttpRequestException e)
{
    Console.WriteLine("Hubo un error al enviar la solicitud: " + e.Message);
}
```

## Ver también
- [Documentación de Microsoft sobre HttpClient Class (en inglés)](https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient?view=net-5.0)
- [Tutorial de C# sobre solicitudes HTTP (en español)](https://www.tutorialsteacher.com/articles/csharp-rest-client-example)
- [Guía para manejar errores de conexión con HttpClient (en inglés)](https://www.thecodebuzz.com/handling-exceptions-httpclient-c/)