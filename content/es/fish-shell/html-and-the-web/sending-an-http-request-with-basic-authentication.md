---
date: 2024-01-20 18:01:22.390881-07:00
description: "Enviar una solicitud HTTP con autenticaci\xF3n b\xE1sica es cuando accedemos\
  \ a recursos protegidos en la web usando un nombre de usuario y contrase\xF1a.\u2026"
lastmod: '2024-03-11T00:14:33.337495-06:00'
model: gpt-4-1106-preview
summary: "Enviar una solicitud HTTP con autenticaci\xF3n b\xE1sica es cuando accedemos\
  \ a recursos protegidos en la web usando un nombre de usuario y contrase\xF1a.\u2026"
title: "Enviando una solicitud http con autenticaci\xF3n b\xE1sica"
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
