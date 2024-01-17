---
title:                "Enviando una solicitud http con autenticación básica"
html_title:           "C#: Enviando una solicitud http con autenticación básica"
simple_title:         "Enviando una solicitud http con autenticación básica"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

Qué y por qué?

Enviar una solicitud HTTP con autenticación básica es un proceso que permite a los programadores realizar solicitudes a servidores web que requieren autenticación a través de un nombre de usuario y una contraseña. Los programadores lo hacen para acceder a recursos protegidos en el servidor y realizar operaciones seguras.

Cómo hacerlo:

En C#, podemos enviar una solicitud HTTP con autenticación básica utilizando la clase HttpClient de la librería System.Net.Http. Primero, debemos inicializar un objeto de esta clase y configurarlo para que acepte autenticación básica. Luego, podemos construir nuestra solicitud y enviarla al servidor. A continuación se muestra un ejemplo de código que utiliza una URL y credenciales de autenticación básica ficticias para demostrar cómo se realiza este proceso:

```C#
// Inicializar HttpClient
HttpClient cliente = new HttpClient();

// Configurar autenticación básica
cliente.DefaultRequestHeaders.Authorization = new AuthenticationHeaderValue("Basic", Convert.ToBase64String(System.Text.Encoding.ASCII.GetBytes("nombre de usuario:contraseña")));

// Construir solicitud y enviarla
HttpResponseMessage respuesta = await cliente.GetAsync("https://ejemplo.com/recurso");
string contenido = await respuesta.Content.ReadAsStringAsync();
Console.WriteLine(contenido);
```

El resultado de este código sería la respuesta del servidor en formato de cadena.

Profundizando:

La autenticación básica fue una forma común de autenticación en los primeros días de la web. Sin embargo, debido a que las contraseñas se envían en texto plano, se considera una forma insegura de autenticación. Como alternativa, se recomienda utilizar autenticación más segura, como HTTPS o OAuth.

Véase también:

Más información sobre la clase HttpClient y la autenticación básica en C#: https://docs.microsoft.com/es-es/dotnet/api/system.net.http.httpclient?view=net-5.0
Una explicación de la diferencia entre autenticación básica, HTTPS y OAuth: https://www.imperva.com/learn/application-security/difference-between-http-https-oauth/