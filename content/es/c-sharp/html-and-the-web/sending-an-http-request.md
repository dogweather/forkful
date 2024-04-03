---
date: 2024-01-20 17:59:12.370760-07:00
description: "Enviar una solicitud HTTP significa pedir datos de un servidor web desde\
  \ tu programa. Los programadores lo hacemos para interactuar con APIs web, recoger\u2026"
lastmod: '2024-03-13T22:44:59.075311-06:00'
model: gpt-4-1106-preview
summary: Enviar una solicitud HTTP significa pedir datos de un servidor web desde
  tu programa.
title: Enviando una solicitud http
weight: 44
---

## ¿Qué & Por Qué?

Enviar una solicitud HTTP significa pedir datos de un servidor web desde tu programa. Los programadores lo hacemos para interactuar con APIs web, recoger datos para nuestras aplicaciones o enviar información a otros servicios.

## Cómo hacerlo:

```C#
using System;
using System.Net.Http;
using System.Threading.Tasks;

class Program
{
    static async Task Main(string[] args)
    {
        using (var httpClient = new HttpClient())
        {
            try
            {
                // GET request
                HttpResponseMessage response = await httpClient.GetAsync("http://api.example.com/data");
                response.EnsureSuccessStatusCode();
                string responseBody = await response.Content.ReadAsStringAsync();
                Console.WriteLine(responseBody);

                // POST request
                var postData = new StringContent("{\"name\":\"Tu Nombre\"}", System.Text.Encoding.UTF8, "application/json");
                HttpResponseMessage postResponse = await httpClient.PostAsync("http://api.example.com/submit", postData);
                postResponse.EnsureSuccessStatusCode();
                string postResponseBody = await postResponse.Content.ReadAsStringAsync();
                Console.WriteLine(postResponseBody);
            }
            catch(HttpRequestException e)
            {
                Console.WriteLine("\nException Caught!");
                Console.WriteLine("Message :{0} ",e.Message);
            }
        }
    }
}
```

Output:
```plaintext
{ "data": "Información obtenida con GET" }
{ "result": "Respuesta de la solicitud POST" }
```

## Profundización:

En los años 90, el HTTP se convirtió en el protocolo estándar para la web. Desde entonces, el envío de solicitudes HTTP ha sido fundamental para la interacción en la web, y ha evolucionado con la tecnología. 

Alternativas: Hay diferentes métodos HTTP (GET, POST, PUT, DELETE, etc.), y varios clientes HTTP en C# como WebClient y RestSharp, pero HttpClient es el más moderno y recomendado.

Detalles de implementación: Con `HttpClient`, puedes personalizar headers, manejar timeouts y manejar errores. Usa `async` y `await` para operaciones sin bloqueo en aplicaciones de consola, escritorio o móviles.

## Ver También:

- [Documentación oficial de HttpClient](https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient)
- [Guía de Microsoft sobre solicitudes asíncronas](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/concepts/async/)
- [Uso de RestSharp para solicitudes HTTP](https://restsharp.dev/)
- [Protocolo HTTP en Wikipedia](https://es.wikipedia.org/wiki/Protocolo_de_transferencia_de_hipertexto)
