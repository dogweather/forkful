---
title:                "Enviando una solicitud http con autenticación básica"
html_title:           "Arduino: Enviando una solicitud http con autenticación básica"
simple_title:         "Enviando una solicitud http con autenticación básica"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/powershell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Enviando una solicitud HTTP con autenticación básica usando PowerShell
## ¿Qué y por qué?
Enviar una solicitud HTTP con autenticación básica es solicitar acceso a un recurso protegido en un servidor web utilizando un nombre de usuario y una contraseña codificados en Base64. Los programadores hacen esto para interactuar con APIs web que requieren autenticación.

## Cómo hacerlo:
PowerShell proporciona el cmdlet `Invoke-RestMethod` para enviar solicitudes HTTP. Aquí hay un ejemplo de cómo utilizar la autenticación básica con este cmdlet:

```PowerShell
$user = 'usuario'
$pass = 'contraseña'
$pair = "$($user):$($pass)"
$encoded = [System.Text.Encoding]::ASCII.GetBytes($pair) | Base64Encode
$basicAuthValue = "Basic $encoded"
$headers = @{
    Authorization = $basicAuthValue
}

$response = Invoke-RestMethod -Uri 'http://api.example.com/data' -Method Get -Headers $headers
$response | Format-List
```

En este código, `$user` y `$pass` representan su nombre de usuario y contraseña que se convierten a un solo par String, se codifica en Base64, se antepone con "Basic " y se agrega a los encabezados de la solicitud HTTP. Finalmente, hay una llamada a `Invoke-RestMethod` para enviar la solicitud.

## Inmersión profunda
En los primeros días de la web, la autenticación básica se utilizaba ampliamente debido a su simplicidad. Sin embargo, es inseguro enviar contraseñas en texto plano, por lo que este método solo se debe utilizar junto con HTTPS. En el caso de APIs muy sensibles, se prefieren métodos más seguros como la autenticación basada en token.

Alternativamente, en lugar de `Invoke-RestMethod`, puedes usar el cmdlet `Invoke-WebRequest` que también es compatible con la autenticación básica y ofrece más detalles sobre la respuesta HTTP.

Algunos servicios web pueden requerir otros encabezados además de la autenticación básica. Asegúrate de revisar la documentación de la API para obtener detalles.

## Ver también
1. [Invoke-RestMethod documentation](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-restmethod?view=powershell-7.1)
2. [Invoke-WebRequest documentation](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-webrequest?view=powershell-7.1)
3. [Base64 encoding in PowerShell](https://devblogs.microsoft.com/powershell/encoding-and-decoding-base64/)