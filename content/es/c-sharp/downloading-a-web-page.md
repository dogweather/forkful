---
title:                "Descargando una página web"
html_title:           "Arduino: Descargando una página web"
simple_title:         "Descargando una página web"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Descargar una página web significa recoger y guardar el contenido HTML de un URL específico. Los programadores descargan páginas web para analizar su contenido, copiar datos, obtener información, monitorizar cambios y mucho más.

## Cómo Hacerlo:

Usaremos la clase HttpClient proporcionada por C# para descargar una página web. Asegúrate de importar el espacio de nombres System.Net.Http.

```C#
using System;
using System.Net.Http;
using System.Threading.Tasks;

class Program
{
    private static readonly HttpClient client = new HttpClient();

    static async Task Main(string[] args)
    {
        var responseString = await client.GetStringAsync("http://www.google.com");
        Console.WriteLine(responseString);
    }
}
```
Al ejecutar el código el resultado sería todo el HTML de la página de inicio de Google.

## Más Adentro:

Historicamente, las páginas web eran descargadas utilizando las clases WebClient o HttpWebRequest disponibles en versiones antiguas de .NET. Sin embargo, HttpClient es la opción moderna y recomendada para la descarga de páginas web en C# a partir de .NET 4.5. Aunque las otras opciones siguen estando disponibles, HttpClient presenta mejor rendimiento y es más fácil de usar.

Existen otras alternativas como RestSharp y Flurl que proporcionan una interfaz de más alto nivel, es decir son aún más fáciles de usar, pero quizás puedan ser menos eficientes en algunos escenarios. 

Al usar HttpClient para descargar páginas web, las solicitudes de HTTP Get se envían al servidor, que responde con el contenido de la página. Este contenido es recogido y almacenado en una cadena, que luego puede ser manipulada o analizada como sea necesario.

## Ver También:

Para más detalles sobre cómo descargar páginas web en C#, visita los enlaces siguientes:
- Documentación oficial de HttpClient: https://docs.microsoft.com/es-es/dotnet/api/system.net.http.httpclient
- Tutorial más completo en el usando C# para descargar una página web: https://docs.microsoft.com/es-es/dotnet/csharp/tutorials/console-webapiclient
- Comparación entre HttpClient, WebClient y HttpWebRequest: https://www.infoworld.com/article/3199781/when-to-use-webclient-vs-httpclient-vs-httpwebrequest.html