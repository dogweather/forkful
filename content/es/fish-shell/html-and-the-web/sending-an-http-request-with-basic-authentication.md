---
title:                "Enviando una solicitud http con autenticación básica"
aliases:
- /es/fish-shell/sending-an-http-request-with-basic-authentication/
date:                  2024-01-20T18:01:22.390881-07:00
model:                 gpt-4-1106-preview
simple_title:         "Enviando una solicitud http con autenticación básica"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Qué y por qué?

Enviar una solicitud HTTP con autenticación básica es cuando accedemos a recursos protegidos en la web usando un nombre de usuario y contraseña. Programadores lo hacen para interactuar con APIs seguras o acceder a contenido restringido automáticamente.

## Cómo hacerlo:

### Enviar solicitud básica usando `curl` en Fish:

```fish
set -x AUTH (echo -n "usuario:contraseña" | base64)
curl -H "Authorization: Basic $AUTH" https://ejemplo.com/recurso
```

### Salida de muestra:

```fish
{
  "id": 123,
  "content": "Datos protegidos"
}
```

## Profundización

La autenticación básica en HTTP es un método de acceso a servicios web desde los principios del internet. No es la más segura, ya que las credenciales se envían en base64, fácil de decodificar. Alternativas modernas incluyen OAuth y tokens de portador (Bearer tokens), que son más robustos.

Implementar autenticación básica en Fish no difiere mucho de otros shells. Usa `curl` con encabezados HTTP para pasar credenciales codificadas. Fish ofrece un enfoque limpio y script-friendly, favoreciendo la claridad sobre la verbosidad.

## Véase también

- Documentación de `curl`: https://curl.se/docs/
- RFC 7617, 'The 'Basic' HTTP Authentication Scheme': https://tools.ietf.org/html/rfc7617
- Guía de Autenticación HTTP: https://developer.mozilla.org/es/docs/Web/HTTP/Authentication
