---
title:                "Enviando una solicitud http"
html_title:           "Bash: Enviando una solicitud http"
simple_title:         "Enviando una solicitud http"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/powershell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## ¿Qué y Por qué?

Las solicitudes HTTP son una comunicación básica entre un cliente (tu ordenador) y un servidor (donde se almacenan los datos). Los programadores las utilizan para obtener información útil para sus aplicaciones, como información de un servicio de clima o post de un blog.

## Cómo hacerlo:

Aquí te mostraremos cómo enviar una solicitud HTTP utilizando PowerShell. Para iniciar, necesitaremos el módulo `Invoke-WebRequest`. Asegúrate de tener PowerShell 3.0 o más reciente.

```PowerShell
#URL a la que enviar la solicitud
$url = "http://example.com"

#La solicitud en sí
$response = Invoke-WebRequest -Uri $url 

#Mostrando el contenido de la respuesta
$response.Content
```

Tu respuesta podría parecer algo como:

```
HTML content here...
```

## Profundizando

Las solicitudes HTTP han existido desde el inicio de la web, son un componente esencial en la comunicación en línea. Aunque utilizamos `Invoke-WebRequest` en PowerShell, hay muchas otras formas de enviar solicitudes HTTP, como usando `curl` en Linux o `HttpClient` en .NET.

Además, las solicitudes HTTP tienen distintos métodos, siendo los más comunes `GET` y `POST`. En nuestro ejemplo, usamos `GET` por defecto, que es para solicitar datos del servidor. Mientras que, `POST` se usa para enviar datos al servidor.

## Ver También:

Para seguir aprendiendo sobre solicitudes HTTP en PowerShell, recomiendo estos enlaces:

- [Documentation of Invoke-WebRequest on Microsoft Docs](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-webrequest)
- [Understanding and using REST APIs](https://www.smashingmagazine.com/2018/01/understanding-using-rest-api/)
- [An introduction to HTTP](https://developer.mozilla.org/en-US/docs/Web/HTTP/Basics_of_HTTP)