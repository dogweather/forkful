---
title:                "Enviando una solicitud http con autenticación básica"
html_title:           "PowerShell: Enviando una solicitud http con autenticación básica"
simple_title:         "Enviando una solicitud http con autenticación básica"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/powershell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

En términos simples, enviar una solicitud HTTP con autenticación básica significa enviar datos a un servidor web utilizando un código de acceso (username y password) para verificar tu identidad. Esto es útil para acceder a recursos restringidos o proteger datos sensibles en tus aplicaciones. Los programadores utilizan este método para autenticar y autorizar a los usuarios en sus aplicaciones web.

## Cómo hacerlo:

##### Código de ejemplo: 

```PowerShell
# Define el código de acceso
$username = "usuario"
$password = "12345"

# Crea un objeto de autenticación
$authStr = "{0}:{1}" -f $username, $password
$encodedAuthStr = [System.Convert]::ToBase64String([System.Text.Encoding]::UTF8.GetBytes($authStr))

# Crea una cabecera de autorización para la solicitud HTTP
$authHeader = "Basic {0}" -f $encodedAuthStr

# Envía la solicitud utilizando Invoke-WebRequest
Invoke-WebRequest -Uri "https://www.miweb.com/api/endpoint" -Headers @{ Authorization = $authHeader } -UseBasicParsing
```

##### Salida de ejemplo:

```
StatusCode        : 200
StatusDescription : OK
Content           : {"message": "Solicitud exitosa"}
RawContent        : HTTP/1.1 200 OK
                    Server: nginx
                    Date: Sat, 24 Jul 2021 00:00:00 GMT
                    Content-Type: application/json
                    Transfer-Encoding: chunked
                    Connection: keep-alive
                    Keep-Alive: ti...
Forms             : {}
Headers           : {[Server, nginx], [Date, Sat, 24 Jul 2021 00:00:00 GMT], [Content-Type, application/json], [Transfer-Encoding, chunked]...}
Images            : {}
InputFields       : {}
Links             : {}
ParsedHtml        : 
RawContentLength  : 
```

## Profundizando:

- Contexto histórico: la autenticación básica es una de las formas más antiguas de autenticación en la web, pero todavía se utiliza ampliamente en aplicaciones y servicios.
- Alternativas: existen otros métodos de autenticación más seguros y complejos, como OAuth, que pueden ser preferidos en ciertos casos.
- Detalles de implementación: el código de acceso debe estar en formato "usuario:password" y luego ser codificado en base64 antes de incluirlo en la cabecera de autorización. También es importante tener en cuenta que los detalles de autenticación deben ser enviados a través de una conexión segura (HTTPS).

## Ver también:

- Documentación de Microsoft: https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-webrequest 
- Artículo sobre autenticación básica: https://www.digitalocean.com/community/tutorials/how-to-use-basic-authentication-in-a-web-application-api