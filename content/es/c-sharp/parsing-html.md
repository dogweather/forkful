---
title:                "Análisis sintáctico de html"
html_title:           "Ruby: Análisis sintáctico de html"
simple_title:         "Análisis sintáctico de html"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/parsing-html.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
El 'parsing' de HTML, extraer y analizar el contenido de un documento HTML, nos permite manipular y utilizar sus datos. Lo hacemos para extraer información, editar la estructura o el contenido, o transformar el documento.

## ¿Cómo Hacerlo?
Para 'parsear' HTML en C#, podemos utilizar la biblioteca HtmlAgilityPack. Aquí hay un ejemplo:

```C#
using HtmlAgilityPack;
HtmlDocument doc = new HtmlDocument();
doc.LoadHtml("<html><body><p>Hola Mundo!</p></body></html>");
var nodo = doc.DocumentNode.SelectSingleNode("//body/p");
Console.WriteLine(nodo.InnerHtml);
```

Salida:
```
Hola Mundo!
```
Este programa carga un HTML, luego busca y selecciona el primer parágrafo en el cuerpo del documento. Finalmente, imprime el contenido del parágrafo - 'Hola Mundo!'.

## Análisis Profundo
'Parsing' HTML ayuda a depurar, poner a prueba y automatizar la web desde los primeros días de la Internet. Sin embargo, HTML no estaba pensado para ser fácil de parsear, llevarlo a cabo puede ser engorroso y propenso a errores.

Además de HtmlAgilityPack, existen otras bibliotecas como CsQuery y AngleSharp. También puedes utilizar regex, pero solo para tareas muy simples. El 'parsing' HTML correctamente requiere una comprensión del HTML y CSS, incluyendo su sintaxis, reglas de precendencia y funcionamiento de etiquetas especiales.

## Ver También
- [HtmlAgilityPack](https://html-agility-pack.net/)
- [CsQuery](https://github.com/jamietre/CsQuery)
- [AngleSharp](https://anglesharp.github.io/)
Examina estos enlaces para obtener más información sobre 'parsing' HTML en C#.