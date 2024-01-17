---
title:                "Analizando html"
html_title:           "C#: Analizando html"
simple_title:         "Analizando html"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/parsing-html.md"
---

{{< edit_this_page >}}

# ¿Qué y por qué?

Parsing HTML es el proceso de analizar y convertir código HTML en una estructura de datos legible para una computadora. Los programadores utilizan el parsing HTML para extraer información específica de una página web y utilizarla en sus aplicaciones.

# Cómo hacerlo:

El siguiente código en C# utiliza la clase ```HtmlAgilityPack``` para realizar el parsing HTML. Primero, asegúrate de tenerla instalada en tu proyecto. Luego, importa el espacio de nombres ```HtmlAgilityPack``` en tu código:

```C#
using HtmlAgilityPack;
```

A continuación, crea una instancia de la clase ```HtmlDocument``` y carga el HTML de una página web en ella:

```C#
HtmlDocument html = new HtmlDocument();
html.Load("https://www.ejemplo.com");
```

Una vez cargado el HTML, puedes utilizar los métodos y propiedades de la clase para acceder a diferentes partes de la página web. Por ejemplo, para obtener una lista de todos los enlaces en la página, puedes utilizar el método ```SelectNodes``` y la sintaxis de XPath:

```C#
HtmlNodeCollection enlaces = html.DocumentNode.SelectNodes("//a");
```

Finalmente, puedes recorrer la colección de nodos para obtener la información que necesites y utilizarla en tu aplicación.

# Profundizando:

El parsing HTML ha sido una herramienta esencial para los programadores desde los inicios de la web. Antes de las tecnologías como HTML5 y AngularJS, que facilitan la creación de páginas web dinámicas, el parsing HTML era la única forma de extraer información de una página.

Aunque el parsing HTML sigue siendo ampliamente utilizado, existen alternativas como el scraping web o el uso de APIs públicas. Además, el proceso puede ser bastante complejo y propenso a errores debido a la naturaleza cambiante del código HTML.

# Ver También:

- [Documentación de HtmlAgilityPack](https://html-agility-pack.net/)
- [Tutorial de parsing HTML en C#](https://www.c-sharpcorner.com/UploadFile/suthish_nair/parsing-html-using-html-agility-pack/)
- [Más sobre scraping web](https://www.scrapehero.com/)