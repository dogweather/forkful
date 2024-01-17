---
title:                "Enviando una solicitud http"
html_title:           "PowerShell: Enviando una solicitud http"
simple_title:         "Enviando una solicitud http"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/powershell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Enviar una solicitud HTTP es una forma común de comunicación entre un cliente y un servidor en la web. Se utiliza para solicitar datos de una página web o para enviar información a un servidor. Los programadores suelen utilizar solicitudes HTTP para interactuar con API's, obtener información de bases de datos y automatizar tareas en línea.

## ¿Cómo hacerlo?
Para enviar una solicitud HTTP en PowerShell, podemos utilizar el cmdlet `Invoke-WebRequest`. Primero, especificamos la dirección URL del sitio web al que deseamos enviar la solicitud:

```PowerShell
$URL = "https://miweb.com/solicitar_datos"
```

Luego, utilizamos `Invoke-WebRequest` junto con el método HTTP correspondiente, por ejemplo, GET, POST, PUT, etc. Para enviar una solicitud GET, escribimos:

```PowerShell
$response = Invoke-WebRequest -Method GET -Uri $URL
```

Esto devolverá una variable `$response` que contendrá toda la información que recibimos del servidor. Podemos acceder a diferentes partes de la respuesta, como el código de estado o el cuerpo del mensaje, utilizando la sintaxis de punto.

```PowerShell
$response.StatusCode # Devuelve el código de estado
$response.Content # Devuelve el cuerpo del mensaje
$response | Select-Object * # Devuelve todas las propiedades de la respuesta
```

## Profundizando
El protocolo HTTP (Hypertext Transfer Protocol) fue creado en 1991 para facilitar la comunicación entre clientes y servidores en la web. Se utiliza ampliamente para transferir datos de diferentes tipos, como HTML, imágenes, videos, etc.

Aparte de `Invoke-WebRequest`, también podemos enviar solicitudes HTTP en PowerShell utilizando otras herramientas como `System.Net.WebClient` y `System.Net.Http.HttpClient`. Estas opciones ofrecen más flexibilidad y control sobre la solicitud, pero pueden requerir más código.

Es importante tener en cuenta que al enviar una solicitud HTTP, también podemos especificar parámetros y encabezados adicionales, como autenticación, contenido, etc. Esto puede variar según el sitio web al que estemos enviando la solicitud.

## Ver también
- Documentación oficial de Microsoft sobre `Invoke-WebRequest`: https://docs.microsoft.com/es-es/powershell/module/microsoft.powershell.utility/invoke-webrequest?view=powershell-7
- Tutorial de código de Microsoft sobre cómo enviar solicitudes HTTP en PowerShell: https://developer.microsoft.com/es-es/scripting/articles/sending-http-requests/

¡Ahora estás listo para enviar solicitudes HTTP en PowerShell y trabajar con datos en la web de manera eficiente y automatizada! ¡A codificar se ha dicho!