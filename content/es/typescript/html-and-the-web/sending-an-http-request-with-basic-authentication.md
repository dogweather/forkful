---
title:                "Enviando una solicitud http con autenticación básica"
aliases:
- /es/typescript/sending-an-http-request-with-basic-authentication/
date:                  2024-01-20T18:02:44.713444-07:00
model:                 gpt-4-1106-preview
simple_title:         "Enviando una solicitud http con autenticación básica"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Enviar una solicitud HTTP con autenticación básica significa incluir credenciales de usuario y contraseña codificadas en la cabecera de la solicitud. Los programadores lo hacen para acceder a recursos que requieren identificación de manera sencilla y rápida.

## Cómo:

```TypeScript
import axios from 'axios';

// Codificar las credenciales en base64
const username = 'usuario';
const password = 'contraseña';
const basicAuth = 'Basic ' + Buffer.from(username + ':' + password).toString('base64');

// Configuración del cliente HTTP con la cabecera de autenticación
const config = {
  headers: {
    'Authorization': basicAuth
  }
};

// Enviar solicitud GET con autenticación básica
axios.get('https://api.ejemplo.com/datos', config)
  .then(response => {
    console.log('Datos recibidos:', response.data);
  })
  .catch(error => {
    console.error('Error al realizar la solicitud:', error);
  });
```

Salida de muestra:

```
Datos recibidos: { ...datos del servidor... }
```

## Análisis Profundo
La autenticación básica es un método antiguo de HTTP para controlar el acceso. No es la opción más segura, ya que las credenciales pueden ser interceptadas fácilmente si no se usa HTTPS. Una alternativa sería usar tokens de autenticación, como OAuth. A nivel de implementación, TypeScript no maneja directamente las solicitudes HTTP; usamos axios o fetch API para esto. Importante: siempre use HTTPS cuando envíe credenciales para evitar exposiciones.

## Ver También
- Documentación de HTTP Basic Authentication en Mozilla Developer Network (MDN): [HTTP authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
- Librería axios para solicitudes HTTP: [Axios on GitHub](https://github.com/axios/axios)
- Fetch API: [Using Fetch - MDN](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API/Using_Fetch)
- Guía de seguridad con tokens de autenticación: [The OAuth 2.0 Authorization Framework](https://oauth.net/2/)
