---
title:                "Elixir: Enviando una solicitud http"
simple_title:         "Enviando una solicitud http"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Por qué

Enviar solicitudes HTTP es una habilidad importante para cualquier desarrollador de Elixir. Puede permitirle interactuar con diferentes servicios web para obtener o enviar información. También es una forma de crear aplicaciones más dinámicas y conectadas.

## Cómo hacerlo

Para enviar una solicitud HTTP en Elixir, primero necesitará instalar la biblioteca HTTPoison. Puede hacerlo agregando `HTTPoison` a su archivo `mix.exs` y luego ejecutando `mix deps.get` en su terminal. Una vez que la biblioteca esté instalada, puede comenzar a utilizarla en su código.

Aquí hay un ejemplo de cómo enviar una solicitud GET utilizando HTTPoison:

```Elixir
HTTPoison.get("https://mi.pagina.com/amigos")
|> IO.inspect # Esto le muestra la respuesta HTTP en la consola
```

En este ejemplo, estamos enviando una solicitud GET a la URL `https://mi.pagina.com/amigos`. La respuesta de la solicitud se imprime en la consola utilizando `IO.inspect`. Puede ver que la sintaxis de Elixir es muy clara y fácil de leer.

También puede enviar solicitudes POST y agregar encabezados y datos a su solicitud utilizando la función `post/3` de HTTPoison.

Para obtener más información sobre cómo enviar una solicitud HTTP utilizando HTTPoison, consulte su documentación oficial aquí: https://hexdocs.pm/httpoison/readme.html

## Profundizando

Además de HTTPoison, Elixir también tiene la biblioteca `Tesla` que se enfoca en el manejo de solicitudes HTTP de manera más eficiente. También puede utilizar la biblioteca `plug` para construir su propio middleware HTTP en Elixir.

Utilizar Elixir para enviar solicitudes HTTP también puede ser útil en la creación de chatbots y aplicaciones de Internet de las cosas (IoT).

## Ver También

- Documentación de HTTPoison: https://hexdocs.pm/httpoison/readme.html
- Documentación de Tesla: https://hexdocs.pm/tesla/readme.html
- Documentación de Plug: https://hexdocs.pm/plug/readme.html