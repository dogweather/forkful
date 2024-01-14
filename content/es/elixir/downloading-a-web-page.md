---
title:                "Elixir: Descargando una página web."
simple_title:         "Descargando una página web."
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Por qué

¿Alguna vez has querido escribir un programa que descargue una página web desde Internet? ¿Quizás quieras extraer información de un sitio web o simplemente guardar una copia local de la página para leerla más tarde. Sea cual sea tu razón, en este artículo te mostraré cómo usar Elixir para descargar una página web y obtener su contenido.

## Cómo hacerlo

Para descargar una página web en Elixir, necesitarás usar la librería HTTPoison. Primero, asegúrate de que tengas instalado Elixir y luego sigue estos pasos:

1. Instala HTTPoison ejecutando `mix deps.get` en la terminal.
2. Importa HTTPoison en tu archivo de Elixir: `import HTTPoison`.
3. Usa la función `HTTPoison.get/2` para hacer una solicitud HTTP. Este método acepta dos argumentos: la URL de la página y una lista de opciones. Por ejemplo:

```
Elixir
response = HTTPoison.get("https://www.google.com", [])
```

4. La respuesta será un mapa con información sobre la solicitud. Para obtener el contenido de la página, puedes acceder al valor `body` del mapa: `response.body`.
5. ¡Eso es todo! Ahora tienes el contenido de la página en una variable y puedes hacer lo que quieras con él.

## Inmersión profunda

Ahora, si quieres entender un poco más sobre cómo funciona la descarga de una página web en Elixir, aquí hay algunas cosas a tener en cuenta:

- HTTPoison utiliza la librería hackney detrás de escena para hacer la solicitud HTTP.
- Puedes agregar opciones adicionales a `HTTPoison.get/2`, como encabezados personalizados o configuración de proxy.
- Si la solicitud es exitosa, obtendrás un código de respuesta `200`. Si hay algún error, como una página no encontrada, obtendrás un código de respuesta `404`.
- HTTPoison también te permite hacer otras solicitudes HTTP, como `POST` o `PUT`.

Con esto en mente, puedes personalizar tus solicitudes HTTP y mejorar tu habilidad para descargar páginas web con Elixir.

## Ver también

- [Documentación oficial de HTTPoison](https://hexdocs.pm/httpoison/HTTPoison.html)
- [Guía de Elixir: cómo usar HTTPoison](https://elixir-lang.org/getting-started/mix-otp/introduction-to-mix.html#fetching-data-over-http)
- [Elixir School: HTTP con Elixir](https://elixirschool.com/en/lessons/specifics/http/)