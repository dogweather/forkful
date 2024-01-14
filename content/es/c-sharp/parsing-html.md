---
title:                "C#: Analizando html"
simple_title:         "Analizando html"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/parsing-html.md"
---

{{< edit_this_page >}}

## Por qué

Hay varias razones por las que alguien podría querer o necesitar analizar HTML en su programa de C#. Quizás quiera extraer datos específicos de una página web o simplemente validar la estructura de un documento HTML antes de mostrarlo en su aplicación. Independientemente de la razón, la capacidad de analizar HTML es una habilidad útil para cualquier programador de C#.

## Cómo hacerlo

Para analizar HTML en C#, utilizaremos la clase `HtmlDocument` de la biblioteca `System.Xml` incorporada. Primero, necesitaremos importar esta biblioteca en nuestro código:

```C#
using System.Xml;
```

Luego, podemos crear una nueva instancia de `HtmlDocument` y cargar un archivo HTML existente o una cadena de texto que contenga el código HTML:

```C#
// Crear una nueva instancia de HtmlDocument
HtmlDocument documento = new HtmlDocument();

// Cargar un archivo HTML existente
documento.Load("archivo.html");

// Cargar una cadena de texto con código HTML
string html = "<html><body><p>Hola mundo!</p></body></html>";
documento.LoadHtml(html);
```

Una vez que tenemos nuestro documento cargado, podemos navegar a través de él utilizando varios métodos y propiedades. Por ejemplo, podemos obtener el elemento `p` mencionado en el ejemplo anterior y mostrar su contenido en la consola:

```C#
// Obtener el elemento p
var elemento = documento.DocumentNode.SelectSingleNode("//p");

// Mostrar su contenido en la consola
Console.WriteLine(elemento.InnerText); // Resultado: Hola mundo!
```

Además de `SelectSingleNode()`, también podemos utilizar otros métodos como `SelectNodes()` o `GetElementbyId()` para obtener elementos específicos del documento HTML. También podemos modificar los elementos existentes o crear nuevos utilizando las propiedades disponibles en la clase `HtmlDocument`.

## Profundizando

Aunque el uso básico de `HtmlDocument` es bastante sencillo, hay muchas más características y funcionalidades disponibles para aquellos que deseen profundizar en el análisis de HTML en C#. Algunos recursos útiles para explorar incluyen:

- [Documentación de Microsoft sobre HtmlDocument](https://docs.microsoft.com/en-us/dotnet/api/system.xml.htmldocument?view=netframework-4.8)
- [Artículo sobre análisis y modificación de HTML con C#](https://www.codeproject.com/Articles/96552/Manipulate-HTML-Document-with-Csharp)
- [Tutorial interactivo sobre manipulación de HTML con C#](https://www.w3schools.com/cs/exercise.asp?filename=exercise_html_dom_create1)

## Ver también

- [Clase HtmlDocument en la documentación oficial de Microsoft](https://docs.microsoft.com/en-us/dotnet/api/system.xml.htmldocument?view=netframework-4.8)
- [Ejemplo de análisis de HTML con C# en GeeksforGeeks](https://www.geeksforgeeks.org/how-to-parse-and-modify-html-in-c-sharp/)
- [Tutorial sobre manipulación de HTML en C# en CodeProject](https://www.codeproject.com/Articles/96552/Manipulate-HTML-Document-with-Csharp)