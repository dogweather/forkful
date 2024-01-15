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

##¿Por qué?

Enviar solicitudes HTTP es una parte fundamental en el desarrollo de aplicaciones web. Al enviar una solicitud HTTP, podemos comunicarnos con servidores remotos y obtener o enviar datos a través de internet.

##¿Cómo hacerlo?

Para enviar una solicitud HTTP en C#, primero debemos crear un objeto HttpClient y especificar la URL del servidor al que deseamos enviar la solicitud. Luego, podemos utilizar diferentes métodos para personalizar nuestra solicitud, como agregar encabezados, parámetros o contenido. Finalmente, utilizamos el método `GetAsync` o `PostAsync` para enviar la solicitud y esperamos una respuesta del servidor.

```
HttpClient client = new HttpClient();
string url = "https://ejemplo.com/api/usuarios";

// Agregar encabezados
client.DefaultRequestHeaders.Add("Authorization", "Token");

// Agregar parámetros
var parametros = new Dictionary<string, string>()
{
    {"nombre", "Juan"},
    {"apellido", "Pérez"},
};
var contenido = new FormUrlEncodedContent(parametros);

// Enviar una solicitud GET
var response = await client.GetAsync(url);

// Enviar una solicitud POST
var response = await client.PostAsync(url, contenido);
```

La respuesta del servidor se guarda en un objeto HttpResponseMessage, del cual podemos obtener información como el código de estado, encabezados y contenido. También podemos utilizar el método `ReadAsStringAsync()` para obtener la respuesta como una cadena de texto.

```
int codigoEstado = (int)response.StatusCode; // Código de estado de la solicitud
string respuesta = await response.Content.ReadAsStringAsync(); // Respuesta del servidor como una cadena de texto
```

##Profundizando

Existen diferentes tipos de solicitudes HTTP que podemos enviar utilizando este método, como GET, POST, PUT y DELETE. Cada solicitud tiene un propósito específico y utiliza diferentes métodos y parámetros para personalizarla. Además, podemos utilizar librerías externas, como RestSharp, para facilitar el proceso de enviar solicitudes HTTP en nuestras aplicaciones.

¿Necesitas más información sobre cómo enviar solicitudes HTTP en C#? Consulta la documentación oficial de Microsoft o busca tutoriales en línea para obtener más detalles y ejemplos prácticos.

##Ver también

- [Documentación de Microsoft de HttpClient en C#](https://docs.microsoft.com/es-es/dotnet/api/system.net.http.httpclient?view=net-5.0)
- [Tutorial de Envío de Solicitudes HTTP en C#](https://www.freecodecamp.org/news/an-introduction-to-http-requests-in-c-sharp/)
- [Librería RestSharp para enviar solicitudes HTTP en C#](https://restsharp.dev/)