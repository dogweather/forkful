---
title:                "Interpretando html"
html_title:           "Elixir: Interpretando html"
simple_title:         "Interpretando html"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/parsing-html.md"
---

{{< edit_this_page >}}

## Por qué

Si eres un desarrollador web, es casi seguro que en algún momento tendrás la tarea de extraer información de una página web. Aquí es donde entra en juego el análisis de HTML. En lugar de hacerlo manualmente, puedes aprovechar la potencia de Elixir para automatizar el proceso.

## Cómo hacerlo

El código Elixir para analizar HTML es bastante sencillo y eficiente. Primero, necesitarás instalar y requerir la librería `Floki`, que nos permite navegar y manipular el DOM de forma fácil y legible:

```Elixir
# Instalación
mix deps.get

# Requerir la librería
require Floki
```

Luego, simplemente debes obtener el HTML de la página que deseas analizar, ya sea a través de una petición HTTP o leyéndolo desde un archivo, y pasarlo como parámetro a la función `Floki.parse/1`:

```Elixir
# Ejemplo de petición HTTP
html = HTTPoison.get!("https://miweb.com").body

# O leerlo desde un archivo
html = File.read!("mipagina.html")

# Parsear el HTML con Floki
parsed = Floki.parse(html)
```

Ahora, para extraer la información que necesitas, puedes utilizar el poderoso patrón de coincidencia de Elixir en combinación con los selectores CSS de Floki. Por ejemplo, si quieres obtener todos los enlaces de la página, puedes hacer lo siguiente:

```Elixir
# Utilizando patrón de coincidencia
for %{tag: :a, href: link} <- parsed, do: link

# Utilizando selectores CSS
Floki.find(parsed, "a")
|> Floki.attribute("href")
```

## Profundizando

La librería Floki utiliza la librería `html_entities`, lo que nos permite manejar entidades HTML en los nodos del DOM. Además, también podemos utilizar expresiones regulares y la función `Floki.match?/2` para realizar búsquedas más precisas en el HTML.

## Ver también

- Documentación de Floki: https://hexdocs.pm/floki/readme.html
- Librería `html_entities`: https://hexdocs.pm/html_entities/readme.html