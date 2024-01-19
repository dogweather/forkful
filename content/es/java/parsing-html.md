---
title:                "Análisis sintáctico de html"
html_title:           "Ruby: Análisis sintáctico de html"
simple_title:         "Análisis sintáctico de html"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/parsing-html.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?
Parsear HTML es el proceso de analizar un documento HTML y convertirlo en un objeto entendible para el programa. Los programadores lo hacen para interactuar y manipular el HTML, permitiéndoles por ejemplo, extraer datos y realizar automatizaciones web.

## ¿Cómo se hace?
Aquí te presento un ejemplo utilizando la biblioteca Jsoup en Java para parsear un HTML. Mira cómo convertimos un documento HTML en un objeto `Document` y luego extraemos datos.

```Java
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;

public class Main {
    public static void main(String[] args) {
        String html = "<html><head><title>Mi primera página web</title></head>"
            + "<body><p>¡Hola, mundo!</p></body></html>";

        Document doc = Jsoup.parse(html);
        Element body = doc.body();
        String texto = body.text();

        System.out.println(texto);  // Imprime: ¡Hola, mundo!
    }
}
```

## Buceo Profundo
1. Contexto histórico: Parsear HTML no es algo nuevo. Ya en los años 90, con el auge de la web, los programadores empezaron a analizar HTML para interactuar con las páginas web.
2. Alternativas: Jsoup es solo una biblioteca de Java para parsear HTML. Otras opciones populares incluyen HtmlCleaner y Jericho.
3. Detalles de implementación: Parsear HTML puede resultar en una estructura tipo árbol conocida como DOM (Modelo de Objetos del Documento). Este modelo permite a los programas manipular el contenido, la estructura y el estilo de los documentos web.

## Ver También
- [Documentación oficial de Jsoup](https://jsoup.org/)
- [Tutorial de HTML parsing con HtmlCleaner](http://htmlcleaner.sourceforge.net/)
- [Guía del W3C sobre el modelo DOM](https://www.w3.org/TR/DOM-Level-2-Core/introduction.html)
Por fa, visita estos enlaces para aprender más sobre el análisis de HTML.