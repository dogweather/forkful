---
title:                "Envío de una solicitud http con autenticación básica"
html_title:           "Elixir: Envío de una solicitud http con autenticación básica"
simple_title:         "Envío de una solicitud http con autenticación básica"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Por qué

En el mundo del desarrollo web, es común que nos encontremos con la necesidad de autenticar a los usuarios para acceder a ciertos recursos. Una forma de hacerlo es a través de la autenticación básica en las solicitudes HTTP. Esta es una forma sencilla pero efectiva de validar las credenciales de un usuario para permitirle acceder a ciertas partes de una aplicación.

## Cómo hacerlo

Para enviar una solicitud HTTP con autenticación básica en Elixir, debemos seguir los siguientes pasos:

1. Primero, debemos agregar el módulo `:hackney` a nuestro archivo `mix.exs` como dependencia: `{:hackney, "~> 1.18"}`.

2. Luego, debemos importar el módulo `:hackney` en nuestro archivo `.ex`: `:hackney` en la sección `:applications` y `:hackney` en la sección `:extra_applications`.

3. Ahora, podemos usar la función `:httpc.request/4` para enviar nuestra solicitud HTTP. Esta función toma cuatro argumentos: el método HTTP, la URL, los encabezados y el cuerpo de la solicitud. Para agregar autenticación básica, debemos incluir un encabezado `Authorization` con las credenciales del usuario codificadas en base64. A continuación, se muestra un ejemplo de cómo hacerlo en Elixir:

```Elixir
:hackney.request(:get, "https://www.example.com", [{"Authorization", "Basic dXNlcm5hbWU6cGFzc3dvcmQ="}], "")
```

4. Finalmente, podemos utilizar el resultado de esta función para obtener la respuesta del servidor. Por ejemplo, podemos imprimir el cuerpo de la respuesta de esta manera:

```Elixir
resp = :hackney.request(:get, "https://www.example.com", [{"Authorization", "Basic dXNlcm5hbWU6cGFzc3dvcmQ="}], "")
IO.puts resp.body
```

Este es solo un ejemplo básico de cómo enviar una solicitud HTTP con autenticación básica en Elixir. Se recomienda explorar la documentación de `:hackney` para obtener más información y opciones avanzadas.

## Profundizando

La autenticación básica en las solicitudes HTTP es una forma simple de verificar las credenciales de un usuario antes de permitir el acceso a ciertos recursos. Sin embargo, es importante tener en cuenta que esta forma de autenticación no es segura por sí sola. Las credenciales del usuario se enviarán en texto plano, lo que significa que pueden ser interceptadas y utilizadas por terceros.

Por lo tanto, se recomienda utilizar HTTPS junto con la autenticación básica para asegurar la seguridad de las credenciales del usuario. También puede ser una buena idea utilizar un formato de autenticación más seguro, como OAuth, en lugar de la autenticación básica.

Es importante considerar la seguridad al utilizar la autenticación básica en las solicitudes HTTP. Siempre es recomendable buscar formas más seguras de proteger los datos de los usuarios.

## Ver también

- Documentación oficial de `:hackney`: https://hexdocs.pm/hackney/readme.html
- Tutorial de autenticación básica en Elixir: https://supergloo.com/fieldnotes/elixir-basic-http-authentication/