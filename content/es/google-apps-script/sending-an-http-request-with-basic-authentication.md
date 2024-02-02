---
title:                "Enviando una solicitud HTTP con autenticación básica"
date:                  2024-02-01T22:01:55.395625-07:00
model:                 gpt-4-0125-preview
simple_title:         "Enviando una solicitud HTTP con autenticación básica"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/google-apps-script/sending-an-http-request-with-basic-authentication.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Qué y Por Qué

Enviar una solicitud HTTP con autenticación básica implica codificar un nombre de usuario y contraseña en un encabezado de solicitud para acceder a recursos protegidos. Los programadores usan este método para la autenticación del lado del servidor, para integrarse con APIs que requieren autenticación básica para operaciones como la recuperación de datos o la publicación de contenido.

## Cómo hacerlo:

En Google Apps Script, para enviar una solicitud HTTP con autenticación básica, utilizas el servicio `UrlFetchApp` combinado con un encabezado de autorización codificado en base64. Aquí tienes una guía paso a paso:

1. **Codificar Credenciales**: Primero, codifica tu nombre de usuario y contraseña en base64. Google Apps Script no tiene una función nativa de codificación base64 para cadenas, así que usarás Utilities.base64Encode para este propósito.

```javascript
var username = 'TuNombreDeUsuario';
var password = 'TuContraseña';
var encodedCredentials = Utilities.base64Encode(username + ':' + password);
```

2. **Configurar Opciones de Solicitud**: Con las credenciales codificadas listas, prepara el objeto de opciones para la solicitud HTTP, incluyendo el método y los encabezados.

```javascript
var options = {
  method: 'get', // o 'post', 'put', dependiendo de tus necesidades
  headers: {
    'Authorization': 'Basic ' + encodedCredentials
  }
  // opciones adicionales como 'muteHttpExceptions' para el manejo de errores se pueden agregar aquí
};
```

3. **Realizar la Solicitud**: Usa el método `UrlFetchApp.fetch` con la URL objetivo y el objeto de opciones.

```javascript
var url = 'https://ejemplo.com/api/recurso';
var response = UrlFetchApp.fetch(url, options);
Logger.log(response.getContentText());
```

El resultado de la muestra tras una solicitud exitosa variará según la respuesta de la API. Para una API basada en JSON, podrías ver algo como:

```
{"status":"Success","data":"Datos del recurso aquí..."}
```

Asegúrate de manejar posibles errores HTTP revisando el código de respuesta o usando la opción `muteHttpExceptions` para un manejo de errores más controlado.

## Exploración Profunda

Enviar una solicitud HTTP con autenticación básica ha sido un método estándar en muchos lenguajes de programación para acceder a recursos basados en la web que requieren autenticación. En el contexto de Google Apps Script, `UrlFetchApp` ofrece una manera directa de realizar estas solicitudes HTTP, incluidas aquellas que requieren autenticación. La inclusión de credenciales básicas en los encabezados de solicitud es un método simple pero efectivo, pero viene con advertencias de seguridad, principalmente porque las credenciales se envían en texto plano, solo codificadas en base64, lo que puede ser fácilmente decodificado si se intercepta.

Para mejorar la seguridad, se recomiendan alternativas como OAuth 2.0, especialmente cuando se trata de datos sensitivos u operaciones. Google Apps Script tiene soporte incorporado para OAuth 2.0 con la biblioteca `OAuth2`, simplificando el proceso de autenticación contra servicios que admiten este protocolo.

A pesar de sus limitaciones de seguridad, la autenticación básica sigue siendo ampliamente utilizada para aplicaciones simples o internas no expuestas a Internet en general. Es sencillo de implementar, ya que requiere solo una solicitud única con encabezados correctamente establecidos, lo que lo hace una opción atractiva para integraciones rápidas o para APIs donde métodos de seguridad más altos no están disponibles. Sin embargo, se insta a los programadores a considerar las implicaciones de seguridad y explorar alternativas más seguras cuando estén disponibles.
