---
title:                "Descargar una página web"
html_title:           "C#: Descargar una página web"
simple_title:         "Descargar una página web"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## ¿Por qué?

Descargar una página web puede tener múltiples motivos, desde querer guardar una copia local para acceso sin conexión hasta para realizar análisis de datos.

## Cómo hacerlo

Para descargar una página web en C#, puedes utilizar la clase WebClient. A continuación, un ejemplo de cómo descargar una página en formato HTML y mostrar su contenido en la consola:

```C#
WebClient client = new WebClient();
string htmlCode = client.DownloadString("https://www.example.com");
Console.WriteLine(htmlCode);
```

La variable "htmlCode" contendrá todo el código HTML de la página descargada, que luego puede ser procesado o guardado en un archivo.

## Profundizando

La clase WebClient también ofrece otras funcionalidades para descargas más avanzadas, como descargar archivos o manejar cookies. Además, puedes especificar datos de autenticación en caso de que la página requiera un login.

Es importante tener en cuenta que, al descargar una página web, se está consumiendo ancho de banda y recursos del servidor. Por lo tanto, es importante ser conscientes del uso que se le dará a dicha descarga.

## Ver también

- Documentación de la clase WebClient (en inglés): https://docs.microsoft.com/en-us/dotnet/api/system.net.webclient?view=netcore-3.1
- Ejemplos de descarga de archivos con WebClient (en inglés): https://www.c-sharpcorner.com/article/downloading-and-uploading-files-with-webclient-in-C-Sharp/