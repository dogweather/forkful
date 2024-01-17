---
title:                "Descargar una página web."
html_title:           "C#: Descargar una página web."
simple_title:         "Descargar una página web."
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# ¿Qué y por qué?

Descargar una página web es el proceso de obtener el código HTML de una página web y guardarlo en tu computadora. Los programadores hacen esto para acceder a la información de la página y usarla en sus aplicaciones.

# Cómo hacerlo:

```C#
using System.Net;

// Crear un objeto para descargar la página
WebClient webClient = new WebClient();

// Descargar el código HTML de la página y guardarlo en una variable
string htmlCode = webClient.DownloadString("https://www.example.com");

// Imprimir el código HTML en la consola
Console.WriteLine(htmlCode);
```

Resultado:

```C#
<!DOCTYPE html>
<html>
<head>
<title>Example Domain</title>
<link rel="stylesheet" href="https://www.example.com/css/bootstrap.min.css">
</head>
<body>
<h1>Example Domain</h1>
<p>This domain is for use in illustrative examples in documents. You may use this domain in literature without prior coordination or asking for permission.</p>
</body>
</html>
```

# Detalles adicionales:

## Contexto histórico:

Descargar páginas web se ha vuelto una práctica común en la era de internet, ya que permite a los desarrolladores acceder a información valiosa y usarla en sus propias aplicaciones.

## Alternativas:

Además de descargar la página completa, también se pueden descargar solo ciertos elementos específicos de una página web, como imágenes o archivos de código CSS. También existen herramientas y bibliotecas específicas diseñadas para descargar páginas web de manera más eficiente y con más funcionalidades.

## Detalles de implementación:

En este ejemplo, se usa la clase `WebClient` de la biblioteca estándar de C# para descargar la página web. Sin embargo, también existen otras formas de hacerlo, como usar la biblioteca de `HttpClient` o bibliotecas de terceros.

# Ver también:

- [WebClient Class](https://docs.microsoft.com/en-us/dotnet/api/system.net.webclient?view=net-5.0)
- [HttpClient Class](https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient?view=net-5.0)
- [AngleSharp - una biblioteca de C# para analizar y manipular código HTML](https://anglesharp.github.io/)