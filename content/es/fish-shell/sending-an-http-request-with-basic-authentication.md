---
title:                "Fish Shell: Enviando una solicitud http con autenticación básica"
simple_title:         "Enviando una solicitud http con autenticación básica"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Por qué
En este blog, vamos a explorar cómo enviar una solicitud HTTP utilizando autenticación básica en Fish Shell. La autenticación es esencial para garantizar que solo los usuarios autorizados puedan acceder a datos y recursos protegidos en línea. Al utilizar la autenticación básica, podemos enviar credenciales de usuario en la solicitud para verificar su identidad y permitir el acceso.

## Cómo hacer
Para enviar una solicitud HTTP con autenticación básica en Fish Shell, necesitamos primero almacenar nuestras credenciales de usuario en una variable. Por ejemplo, podemos almacenar nuestro nombre de usuario en la variable $USERNAME y nuestra contraseña en la variable $PASSWORD.

```Fish Shell
set -x USERNAME usuario
set -x PASSWORD contraseña
```

Luego, utilizaremos la herramienta cURL para enviar la solicitud HTTP. Dentro de nuestro bloque de código, podemos escribir lo siguiente:

```Fish Shell
set -l RESPONSE (curl -u $USERNAME:$PASSWORD http://www.ejemplo.com)
echo $RESPONSE
```

Al utilizar la opción -u en cURL, le indicamos que enviemos nuestras credenciales de usuario en la solicitud. El formato de la opción es usuario:contraseña. En este ejemplo, reemplazamos $USERNAME y $PASSWORD con nuestras variables previamente establecidas.

## Buceo profundo
Es importante tener en cuenta que, al utilizar la autenticación básica, nuestras credenciales se enviarán sin cifrar en la solicitud HTTP. Por lo tanto, es importante utilizar este método solo en casos en que se requiera una seguridad mínima. Para una mayor seguridad, es recomendable utilizar otros métodos de autenticación, como OAuth o certificados SSL.

También es importante mencionar que algunas aplicaciones pueden requerir que las credenciales de autenticación básica se codifiquen en base64 antes de enviar la solicitud. Puedes utilizar la herramienta base64 de Fish Shell para codificar tus credenciales de esta manera.

## Ver también
Si estás interesado en aprender más sobre la autenticación básica en Fish Shell y otras herramientas y técnicas de desarrollo web, consulta los siguientes enlaces:

- [HTTP Authentication Basic](https://developer.mozilla.org/es/docs/Web/HTTP/Authentication#basic_authentication_scheme)
- [Tutorial de Fish Shell](https://fishshell.com/docs/current/tutorial.html)
- [Cómo codificar cadenas en base64 en Fish Shell](https://www.systutorials.com/docs/linux/man/1-fish_base64/)