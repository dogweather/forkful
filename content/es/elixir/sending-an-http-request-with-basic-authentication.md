---
title:                "Enviando una solicitud http con autenticación básica"
html_title:           "Elixir: Enviando una solicitud http con autenticación básica"
simple_title:         "Enviando una solicitud http con autenticación básica"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?
Enviar una solicitud HTTP con autenticación básica es una forma de autenticar un usuario en un servidor web mediante la inclusión de credenciales en la solicitud. Los programadores lo hacen para garantizar la seguridad y privacidad de los usuarios al acceder a ciertas páginas web o recursos en línea.

## Cómo:
Enviar una solicitud HTTP con autenticación básica en Elixir es sencillo. Primero, debes importar el módulo de `HTTPoison` en tu proyecto:
```
Elixir import HTTPoison
```
Luego, puedes usar la función `request/5` para enviar la solicitud con las credenciales adecuadas:
```
Elixir HTTPoison.request("GET", "https://ejemplo.com", [], [], [basic_auth: {"usuario", "contraseña"}])
```
El resultado será una estructura `%HTTPoison.Response{}` que contiene el código de estado, el cuerpo de la respuesta y otras propiedades útiles.

## Profundizando:
La autenticación básica en HTTP fue introducida en 1999 como un medio de autenticación simple en la web. Sin embargo, debido a su naturaleza no cifrada, se considera insegura en comparación con otros métodos de autenticación más modernos como OAuth.

Hay otras formas de autenticación HTTP en Elixir, como la autenticación digest y la autenticación de token. También puedes utilizar un módulo como `Plug.BasicAuth` para implementar la autenticación básica en tu servidor web.

## Ver también:
- [Documentación de HTTPoison](https://hexdocs.pm/httpoison/HTTPoison.html)
- [Tutorial de autenticación básica en Elixir](https://elixirschool.com/es/lessons/advanced/http-basic-auth/)