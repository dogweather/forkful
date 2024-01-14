---
title:                "Javascript: Enviando una solicitud HTTP con autenticación básica"
simple_title:         "Enviando una solicitud HTTP con autenticación básica"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# ¿Por qué enviar una solicitud HTTP con autenticación básica?

Enviar una solicitud HTTP con autenticación básica es una técnica importante en la programación web. Permite a los desarrolladores autenticarse con un servidor para acceder a información protegida o realizar acciones restringidas.

## Cómo hacerlo

Para enviar una solicitud HTTP con autenticación básica en JavaScript, primero debes crear un objeto XMLHttpRequest. Luego, establece el método de solicitud y la URL con el método `open()`. A continuación, agrega encabezados para indicar que se utilizará la autenticación básica con el método `setRequestHeader()`. Por último, envía la solicitud con el método `send()`.

```Javascript
var xhr = new XMLHttpRequest();
xhr.open('GET', 'https://ejemplo.com/datos-protegidos');
xhr.setRequestHeader('Authorization', 'Basic usuario:contraseña');
xhr.send();
```

Esta solicitud GET enviará la información de autenticación en el encabezado `Authorization`. En el lado del servidor, el usuario y la contraseña se pueden verificar para permitir o denegar el acceso.

## Profundizando

La autenticación básica es un método simple pero no seguro de autenticación en HTTP. Esta técnica se considera obsoleta y se recomienda utilizar métodos más seguros y avanzados, como la autenticación con tokens o con el protocolo OAuth.

Además, es importante tener en cuenta que la información de autenticación se envía en texto plano, lo que la hace vulnerable en una conexión no segura. Se recomienda utilizar HTTPS para proteger la información de autenticación y cualquier otra información sensible que se envíe en la solicitud.

# Ver también

- [XMLHttpRequest en MDN](https://developer.mozilla.org/es/docs/Web/API/XMLHttpRequest)
- [Autenticación HTTP en Wikipedia](https://es.wikipedia.org/wiki/Autenticaci%C3%B3n_HTTP)
- [Seguridad en la web en MDN](https://developer.mozilla.org/es/docs/Web/Security)