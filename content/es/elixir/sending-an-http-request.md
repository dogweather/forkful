---
title:                "Enviando una petición http"
html_title:           "Elixir: Enviando una petición http"
simple_title:         "Enviando una petición http"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/sending-an-http-request.md"
---

{{< edit_this_page >}}

# ¡Envío de solicitudes HTTP con Elixir!

## ¿Qué y por qué?

Enviar solicitudes HTTP en Elixir significa establecer una conexión entre la aplicación que estamos construyendo y un servidor web externo. Esta es una forma fundamental para que los programadores soliciten y reciban información en línea, como datos de una API o contenido de una página web.

## Cómo hacerlo:

Elixir proporciona una biblioteca estándar llamada `:httpc` que se puede utilizar para enviar solicitudes HTTP. Para comenzar, debemos importar la biblioteca en nuestro archivo de código:

```elixir
import :httpc
```

Luego, podemos utilizar la función `request/4` para enviar una solicitud. Por ejemplo, si queremos obtener el contenido de la página de inicio de Google, podemos hacer lo siguiente:

```elixir
{:ok, {{_, 200, _}, _, body}} = request(:get, {"https://www.google.com", []}, [], [])
IO.puts body
```

Este código primero realizará una solicitud HTTP GET a la URL especificada y luego utilizará `IO.puts` para imprimir el contenido de la página en la consola.

## Profundizando:

La capacidad de enviar solicitudes HTTP es esencial en el mundo de la programación moderna. Antes de que existiera Elixir, se utilizaba principalmente el lenguaje de programación Erlang para manejar solicitudes HTTP en aplicaciones web. Sin embargo, Elixir ha simplificado enormemente este proceso con su sintaxis y características amigables para el desarrollador.

Si bien `:httpc` es la forma más común de enviar solicitudes HTTP en Elixir, también existen otras alternativas populares como `HTTPoison` y `Tesla`. Cada una ofrece su propio conjunto de características y ventajas, así que asegúrate de elegir la que mejor se adapte a tus necesidades.

En cuanto a los detalles de implementación, Elixir utiliza el módulo `:httpc` de Erlang para enviar solicitudes HTTP. Este módulo ha demostrado ser confiable y robusto a lo largo de los años, lo que garantiza que nuestras solicitudes se realicen de manera eficiente y segura.

## Ver también:

Para obtener más información sobre cómo enviar solicitudes HTTP en Elixir, puedes consultar la documentación oficial de la biblioteca `:httpc` en https://hexdocs.pm/elixir/HTTPc.html. También puedes explorar otros recursos en línea, como tutoriales y ejemplos de código, para obtener una comprensión más profunda de este tema. ¡Feliz codificación!