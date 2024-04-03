---
date: 2024-01-20 15:30:36.963297-07:00
description: "C\xF3mo: Para parsear HTML en C#, puedes usar la librer\xEDa HtmlAgilityPack.\
  \ Aqu\xED te muestro c\xF3mo puedes cargar un documento HTML y seleccionar elementos\u2026"
lastmod: '2024-03-13T22:44:59.076190-06:00'
model: unknown
summary: "Para parsear HTML en C#, puedes usar la librer\xEDa HtmlAgilityPack."
title: "An\xE1lisis de HTML"
weight: 43
---

## Cómo:
Para parsear HTML en C#, puedes usar la librería HtmlAgilityPack. Aquí te muestro cómo puedes cargar un documento HTML y seleccionar elementos específicos por su clase.

```C#
using HtmlAgilityPack;
using System;
using System.Linq;

class Program
{
    static void Main()
    {
        // Carga el HTML desde un archivo o una URL
        var htmlDoc = new HtmlDocument();
        htmlDoc.Load("tuArchivo.html");

        // También puedes cargar el HTML directamente de una URL así:
        // var web = new HtmlWeb();
        // var htmlDoc = web.Load("http://tuUrl.com"); 

        // Selecciona nodos por su clase
        var nodos = htmlDoc.DocumentNode.SelectNodes("//div[@class='tu-clase']");

        foreach (var nodo in nodos)
        {
            Console.WriteLine(nodo.InnerText);
        }
    }
}
```

Salida de muestra:
```
Texto dentro de tu primer div de clase 'tu-clase'
Texto dentro de tu segundo div de clase 'tu-clase'
```

## Profundizando
El parseo de HTML no es algo nuevo. Desde los inicios de la web, ha habido una necesidad constante de leer y manipular HTML. Puedes hacerlo de forma nativa en C# usando clases como `HttpWebRequest` y `HtmlDocument`, pero es rudimentario y propenso a errores sin una librería robusta. HtmlAgilityPack es una de esas bibliotecas, permitiendo navegar por un documento HTML con la misma facilidad que jQuery lo hace en el navegador. Las alternativas en .NET incluyen librerías como CsQuery o AngleSharp. En la implementación, importa la robustez y la resistencia a HTMLs incorrectos; HtmlAgilityPack maneja bien estas situaciones y sigue el modelo del DOM muy al estilo de cómo lo hace jQuery.

## Ver además
- HtmlAgilityPack en GitHub: https://github.com/zzzprojects/html-agility-pack
- Documentación de HtmlAgilityPack: https://html-agility-pack.net/
- Tutorial de AngleSharp: https://anglesharp.github.io/
- Tutorial de CsQuery: https://github.com/jamietre/CsQuery
