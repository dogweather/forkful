---
title:                "Enviando una solicitud http con autenticación básica"
html_title:           "Fish Shell: Enviando una solicitud http con autenticación básica"
simple_title:         "Enviando una solicitud http con autenticación básica"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

¿Qué es y por qué enviar una solicitud HTTP con autenticación básica?

Enviar una solicitud HTTP con autenticación básica significa que estás proporcionando credenciales de inicio de sesión (usuario y contraseña) para poder acceder a una API o servicio web protegido. Los programadores lo hacen para asegurarse de que solo usuarios autorizados tengan acceso a ciertos recursos o datos.

Cómo hacerlo:

```Fish Shell
curl -u username:password url
```

Ejemplo de código:

```Fish Shell
curl -u admin123:secretpass https://api.example.com/users
```

Resultado:

```
Response code: 200
Body: {
  "id": 123,
  "name": "John Doe",
  "email": "johndoe@example.com"
}
```

Profundizando:

Este método de autenticación se ha utilizado en la web desde los primeros días de HTTP. Sin embargo, ha sido criticado por su vulnerabilidad a ataques de replay debido a que las credenciales se envían en texto plano. Alternativas más seguras incluyen el uso de tokens de acceso o TLS (Transport Layer Security).

Además, es importante tener en cuenta que aunque es muy común utilizar una solicitud HTTP con autenticación básica para acceder a una API, no es una práctica recomendada para la autenticación de usuarios en aplicaciones web. En su lugar, se sugiere el uso de un flujo de autenticación más robusto como OAuth.

En cuanto a la implementación, Fish Shell ofrece una herramienta integrada llamada ```curl```para enviar solicitudes HTTP con autenticación básica. Sin embargo, también hay bibliotecas de otros lenguajes de programación que facilitan este proceso, como la biblioteca de Python, ```Requests```.

Consulte también:

- [Insecurity of HTTP Basic Authentication] (http://www.codingdefined.com/2014/07/insecurity-of-http-basic-authentication.html)
- [OAuth vs. HTTP Basic Authentication] (https://medium.com/@paigen11/why-oauth-is-a-better-alternative-than-basic-authentication-b368ceb65176)