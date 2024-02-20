---
date: 2024-01-20 17:43:47.717635-07:00
description: "Descargar una p\xE1gina web es obtener su contenido HTML a trav\xE9\
  s de Internet. Los programadores lo hacen para analizar datos, verificar disponibilidad\
  \ o\u2026"
lastmod: 2024-02-19 22:05:17.288285
model: gpt-4-1106-preview
summary: "Descargar una p\xE1gina web es obtener su contenido HTML a trav\xE9s de\
  \ Internet. Los programadores lo hacen para analizar datos, verificar disponibilidad\
  \ o\u2026"
title: "Descargando una p\xE1gina web"
---

{{< edit_this_page >}}

## ¿Qué & Por Qué?

Descargar una página web es obtener su contenido HTML a través de Internet. Los programadores lo hacen para analizar datos, verificar disponibilidad o automatizar interacciones con la web.

## Cómo hacerlo:

Elixir facilita esta tarea con la librería HTTPoison. Aquí hay un ejemplo:

```elixir
# Primero, añade HTTPoison a tus dependencias en `mix.exs`:
defp deps do
  [
    {:httpoison, "~> 1.8"}
  ]
end

# Luego ejecuta `mix deps.get` para instalarla.

# Ahora puedes usar HTTPoison para descargar el contenido de una página web:
def download_web_page(url) do
  case HTTPoison.get(url) do
    {:ok, %HTTPoison.Response{status_code: 200, body: body}} ->
      IO.puts("¡Descarga exitosa!")
      IO.puts(body)
    {:ok, %HTTPoison.Response{status_code: status_code}} ->
      IO.puts("Se encontró la página pero respondió con estado: #{status_code}")
    {:error, %HTTPoison.Error{reason: reason}} ->
      IO.puts("Error al descargar la página: #{reason}")
  end
end

# Uso:
download_web_page("http://example.com")
```

## Análisis Profundo:

La historia detrás de descargar páginas web se remonta a los primeros días del internet, donde herramientas como cURL y Wget eran comúnmente utilizadas en sistemas Unix. En Elixir, aparte de HTTPoison, existen otras alternativas como Tesla y Finch que también son opciones válidas. La elección entre ellas depende de las preferencias personales y los requerimientos específicos del proyecto.

HTTPoison, que se basa en la librería de Erlang hackney, maneja la complejidad de las conexiones HTTP/HTTPS. Su API es flexible, lo que permite hacer cosas más complejas como manejar cookies, cabeceras y parámetros.

## Ver También:

- Documentación oficial de HTTPoison: [https://hexdocs.pm/httpoison/](https://hexdocs.pm/httpoison/)
- Proyecto GitHub de HTTPoison: [https://github.com/edgurgel/httpoison](https://github.com/edgurgel/httpoison)
- Documentación de Tesla: [https://hexdocs.pm/tesla/readme.html](https://hexdocs.pm/tesla/readme.html)
- Repositorio GitHub de Finch: [https://github.com/sneako/finch](https://github.com/sneako/finch)
- Tutorial de Elixir: [https://elixir-lang.org/getting-started/introduction.html](https://elixir-lang.org/getting-started/introduction.html)
