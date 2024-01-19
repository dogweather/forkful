---
title:                "Enviando una solicitud http con autenticación básica"
html_title:           "Arduino: Enviando una solicitud http con autenticación básica"
simple_title:         "Enviando una solicitud http con autenticación básica"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?
Enviar una solicitud HTTP con autenticación básica implica proporcionar un nombre de usuario y contraseña para acceder a recursos de API restringidos. Los programadores lo hacen para interactuar con APIs seguras que requieren autenticación.

## ¿Cómo hacerlo?
En Elixir, puedes usar la biblioteca HTTPoison para enviar una solicitud HTTP con autenticación básica. Aquí hay un ejemplo:

```Elixir
alias HTTPoison.{BasicAuth, Get}

auth = BasicAuth.encode_credentials("username", "password")

response = 
  Get.stream!("https://myapi.com/endpoint", [], [basic_auth: auth])
  |> Enum.to_list

IO.inspect(response)
```

Esto enviará una solicitud GET a la url especificada con las credenciales proporcionadas. La respuesta se transmite para evitar cargas excesivas en la memoria.

## Deep Dive (Inmersión profunda)
El protocolo de autenticación básica HTTP tiene sus raíces en los primeros días de las aplicaciones web. Aunque es sencillo, no es seguro para las credenciales de texto plano sin una conexión HTTPS.

En el código de Elixir presentado, la función `BasicAuth.encode_credentials/2` toma un nombre de usuario y contraseña y los codifica en el formato requerido para la cabecera Authorization HTTP.

También puedes hacerlo con el módulo `:httpc` de Erlang si prefieres trabajar con los bloques de construcción de más bajo nivel:

```Elixir
:httpc.request(:get, {'https://myapi.com/endpoint', [{'Authorization', 'Basic ' <> :base64.encode_to_string('username' <> ":" <> 'password')}]}, [], [])
```

Si la API se basa en tokens en lugar de en credenciales de usuario, puedes usar la misma técnica pero reemplazando las credenciales de BasicAuth con el token.

## See Also (Ver también)
Para más detalles, consulta los siguientes recursos:
- [Documentación HTTPoison](https://hexdocs.pm/httpoison/readme.html)
- [Módulo BasicAuth](https://hexdocs.pm/httpoison/HTTPoison.BasicAuth.html)
- [Documentación sobre las solicitudes HTTP en Erlang](http://erlang.org/doc/man/httpc.html)