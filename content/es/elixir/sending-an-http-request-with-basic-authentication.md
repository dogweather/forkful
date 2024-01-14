---
title:                "Elixir: Enviando una solicitud http con autenticación básica"
simple_title:         "Enviando una solicitud http con autenticación básica"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Por qué

En la programación de Elixir, a menudo es necesario interactuar con aplicaciones web mediante el envío de solicitudes HTTP. Para garantizar la seguridad y autorización adecuadas, es fundamental comprender cómo enviar una solicitud HTTP con autenticación básica.

## Cómo hacerlo

Enviar una solicitud HTTP con autenticación básica en Elixir es sencillo gracias a la librería HTTPoison. Primero, debemos agregar la dependencia a nuestro archivo mix.exs:

```Elixir
def deps do
  [{:httpoison, "~> 1.6"}]
end
```

Luego, podemos realizar la solicitud utilizando la función `HTTPoison.get/2` y pasando una URL y parámetros opcionales, incluyendo el encabezado de autenticación básica:

```Elixir
{:ok, respuesta} = HTTPoison.get("https://ejemplo.com", [], [basic_auth: {"usuario", "contraseña"}])
```

La respuesta será un tupla con el estado de la solicitud y la respuesta en formato JSON. Podemos acceder a la respuesta utilizando la función `Jason.decode/1`:

```Elixir
cuerpo = respuesta.body |> Jason.decode()
```

## Profundizando

Al realizar una solicitud HTTP con autenticación básica, debemos asegurarnos de incluir el encabezado "Authorization" en nuestra solicitud. Este encabezado debe estar en formato "usuario:contraseña" codificado en base64. Podemos utilizar la función `Base.encode64/1` para codificar el encabezado correctamente.

Además, también es importante tener en cuenta que el servidor puede requerir una autenticación adicional, como una clave de acceso API. En este caso, debemos incluir esta información adicional en la solicitud.

## Ver también

- [Documentación HTTPoison] (https://hexdocs.pm/httpoison/readme.html)
- [Ejemplo de código] (https://blog.lelonek.me/httpoison-goodbye-to-net-http-and-typhoeus-3fb772116fe1)
- [Encabezados HTTP] (https://developer.mozilla.org/es/docs/Web/HTTP/Headers/Authorization)