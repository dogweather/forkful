---
title:                "Enviando una solicitud http con autenticación básica"
html_title:           "TypeScript: Enviando una solicitud http con autenticación básica"
simple_title:         "Enviando una solicitud http con autenticación básica"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Qué y Por qué?
En TypeScript, enviar una solicitud HTTP con autenticación básica se refiere a incluir credenciales de usuario en el encabezado de la solicitud con el fin de acceder a un recurso protegido. Los programadores lo hacen para asegurar que solo los usuarios autorizados tengan acceso a ciertas funciones o datos en una aplicación web.

## Cómo hacerlo:
Para enviar una solicitud HTTP con autenticación básica en TypeScript, podemos utilizar la librería 'axios'. Primero, importamos la librería y luego creamos un objeto de configuración con las credenciales del usuario. A continuación, hacemos la llamada a la solicitud y manejamos la respuesta en una función de retorno de llamada. Aquí hay un ejemplo de código:

```TypeScript
import axios from ‘axios’;

const config = {
  auth: {
    username: 'user',
    password: 'password'
  }
};
axios.get('https://api.example.com/resource', config)
  .then(response => {
    console.log(response.data);
  })
  .catch(error => {
    console.log(error);
  });
```

La respuesta del servidor estará disponible en la propiedad `data` del objeto de respuesta.

## Profundizando:
La autenticación básica HTTP se basa en un estándar muy antiguo y ha sido reemplazada por métodos más seguros como OAuth y JSON Web Tokens. Sin embargo, sigue siendo ampliamente utilizada debido a su simplicidad. Es importante tener en cuenta que enviar credenciales de usuario en texto plano a través de una solicitud HTTP no es completamente seguro, ya que podrían ser interceptadas por un tercero. Por lo tanto, se debe considerar el uso de métodos de autenticación más seguros en aplicaciones que manejan información delicada.

## Ver también:
- Documentación oficial de la librería `axios`: https://axios-http.com/docs/intro
- Artículo de MDN sobre la autenticación básica HTTP: https://developer.mozilla.org/es/docs/Web/HTTP/Basic_access_authentication