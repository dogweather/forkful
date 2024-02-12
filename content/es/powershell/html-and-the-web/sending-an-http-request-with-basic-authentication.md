---
title:                "Enviando una solicitud http con autenticación básica"
aliases:
- /es/powershell/sending-an-http-request-with-basic-authentication/
date:                  2024-01-20T18:02:19.687293-07:00
model:                 gpt-4-1106-preview
simple_title:         "Enviando una solicitud http con autenticación básica"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/powershell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Enviar una petición HTTP con autenticación básica es el proceso de realizar una solicitud a un servicio web requiriendo usuario y contraseña codificados en base64. Los programadores lo hacen para interactuar con APIs o servicios web que requieren seguridad mínima para acceder a recursos protegidos.

## Cómo Hacerlo:
Para enviar una petición con autenticación básica en PowerShell, primero construye las credenciales en base64 y luego úsalas en la cabecera de la petición. Este es un ejemplo sencillo:

```PowerShell
# Usuario y contraseña
$User = "miUsuario"
$Password = "miContraseña"

# Codificar las credenciales en base64
$Base64AuthInfo = [Convert]::ToBase64String([Text.Encoding]::ASCII.GetBytes(("{0}:{1}" -f $User,$Password)))

# Configurar los headers de la petición HTTP
$headers = @{
    Authorization=("Basic {0}" -f $Base64AuthInfo)
}

# La URL del servicio web a llamar
$Url = "https://miapi.com/datos"

# Enviar la petición GET con las credenciales
$response = Invoke-RestMethod -Uri $Url -Method Get -Headers $headers

# Mostrar la respuesta
$response
```
Si todo sale bien, `$response` contendrá los datos proporcionados por el servicio web.

## Profundización:
La autenticación básica es una de las formas más sencillas de seguridad HTTP, introducida en los inicios de la web. Sin embargo, hoy en día se considera insegura si no se usa con HTTPS, ya que las credenciales podrían ser interceptadas fácilmente. A pesar de eso, es común para scripts internos o en redes confiables.

Alternativas más seguras como OAuth2 o tokens de API son preferibles para ambientes de producción. La autenticación básica, sin embargo, sigue siendo relevante para prototipos rápidos o APIs con bajo perfil de riesgo.

A nivel técnico, la autenticación básica envía el usuario y contraseña en `headers` HTTP, codificados en base64. La simplicidad de su implementación es tanto una virtud como un problema de seguridad inherente, ya que depende enteramente de la confidencialidad de la conexión subyacente (TLS/SSL).

## Ver También:
- [Guía oficial de PowerShell sobre Invoke-RestMethod](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-restmethod)
- [Fundamentos de la autenticación HTTP en Mozilla developer network (MDN)](https://developer.mozilla.org/es/docs/Web/HTTP/Authentication)
- [¿Por qué la autenticación básica es considerada insegura?](https://security.stackexchange.com/questions/988/is-basic-auth-secure-if-done-over-https)
- [Autenticación con tokens en APIs REST](https://jwt.io/introduction/)
