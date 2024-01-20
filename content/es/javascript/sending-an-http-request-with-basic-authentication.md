---
title:                "Enviando una solicitud http con autenticación básica"
html_title:           "Arduino: Enviando una solicitud http con autenticación básica"
simple_title:         "Enviando una solicitud http con autenticación básica"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Enviar una solicitud HTTP con autenticación básica es un método simple para confirmar la identidad de un usuario antes de permitir acceso a ciertos datos. Los programadores lo hacen para proteger los datos sensibles y mantener un nivel básico de seguridad.

## Cómo se hace:

Para esto, necesitarás la biblioteca `axios`. Aquí hay un ejemplo rápido:
 
```Javascript
const axios = require('axios');

axios({
  method: 'get',
  url: 'http://api.algunlugar.com/datos',
  auth: {
    username: 'usuario',
    password: 'contraseña'
  }
})
.then(response => {
  console.log(response);
})
.catch(error => {
  console.log(error);
});
```
Este código envía una solicitud HTTP GET a la URL especificada. En la respuesta, obtendrás los datos solicitados.

## Iniciando:

Aunque la autenticación básica es antigua y simple, ha sido una parte integral del protocolo HTTP desde sus inicios. Sin embargo, no es la más segura. Otras alternativas incluyen la autenticación por tokens, OAuth y JWT.

En la autenticación básica, las credenciales son codificadas en Base64 pero no están encriptadas. Esto significa que tu información puede ser interceptada y descifrada. Es por eso que solo debes usarla sobre conexiones HTTPS.

## Ver También:

Por favor, échale un vistazo a estos recursos para más detalles y alternativas:

1. [Autenticación HTTP en MDN](https://developer.mozilla.org/es/docs/Web/HTTP/Authentication)
2. [Autenticación JWT](https://jwt.io/introduction/)
3. [OAuth 2.0](https://oauth.net/2/)