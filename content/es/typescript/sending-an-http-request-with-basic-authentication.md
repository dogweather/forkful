---
title:                "Enviando una solicitud http con autenticación básica"
html_title:           "Arduino: Enviando una solicitud http con autenticación básica"
simple_title:         "Enviando una solicitud http con autenticación básica"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?

Enviar una solicitud HTTP con autenticación básica es una técnica que los desarrolladores usan para transmitir solicitudes y respuestas por la web resguardando información confidencial. Esencialmente, se utiliza encriptación basada en 'Base64' para proteger las credenciales del usuario.

## ¿Cómo hacerlo?

```TypeScript
import axios from "axios";

let nombreUsuario = 'usuario';
let contrasena = 'contrasena';

let base64 = Buffer.from(nombreUsuario + ':' + contrasena, 'utf8').toString('base64');
let config = {
    headers: { 'Authorization': 'Basic ' + base64 }
};

let bodyParameters = {
   key: "value"
};

axios.post(`http://ejemplo.com/api/endpoint`, 
           bodyParameters,
           config)
.then(response => {
    console.log(response.data);
}).catch(error => {
    console.log(error);
});
```

## Hablemos más a fondo

Antes de la era moderna de protección avanzada de datos, la autenticación básica HTTP era el estándar de facto para garantizar el acceso seguro a la web. Aunque su uso ha disminuido en los últimos años debido a métodos más seguros, como los tokens de acceso JWT, aún persiste en ciertas aplicaciones y sistemas heredados.

Aunque la Autenticación Básica HTTP aporta un nivel inicial de seguridad, no debe utilizarse como la única forma de proteger la información sensible del usuario. La encriptación Base64 utilizada en este método puede ser decodificada fácilmente, lo que la hace vulnerable a los ataques "Man-in-the-Middle".

Para una capa adicional de seguridad, siempre se debe transmitir información a través de HTTPS en lugar de HTTP cuando se utiliza la autenticación básica.

## Ver también

- [Axios NPM](https://www.npmjs.com/package/axios) para una detallada documentación sobre el módulo de solicitudes HTTP.
- [MDN Web Docs](https://developer.mozilla.org/es/docs/Web/HTTP/Authentication) para obtener más información sobre la autenticación HTTP.
- [JWT Tokens](https://jwt.io/introduction/) para una introducción a un método más seguro de autenticación.