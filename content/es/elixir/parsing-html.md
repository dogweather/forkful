---
title:                "Análisis de HTML"
date:                  2024-01-20T15:31:13.556744-07:00
simple_title:         "Análisis de HTML"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/parsing-html.md"
---

{{< edit_this_page >}}

## Qué & Por Qué?
Parsear HTML implica convertir un documento HTML en una estructura de datos que un programa puede manipular. Los programadores lo hacen para extraer información, automatizar interacciones web o para el scraping de sitios.

## Cómo hacerlo:
Con Elixir, podemos usar la biblioteca Floki para parsear HTML fácilmente. A continuación, un ejemplo sencillo:

```elixir
# Asegúrate de añadir floki a tus dependencias en mix.exs
defp deps do
  [
    {:floki, "~> 0.32.0"}
  ]
end

# Ejemplo de cómo obtener los enlaces de un documento HTML
def obtener_enlaces(html) do
  html
  |> Floki.parse()
  |> Floki.find("a")
  |> Floki.attribute("href")
end

# Uso del ejemplo
html = "<html><body><a href='https://elixir-lang.org'>Elixir</a></body></html>"
enlaces = obtener_enlaces(html)
IO.inspect(enlaces)
```

Salida de muestra:

```elixir
["https://elixir-lang.org"]
```

## Profundización:
Historicamente, parsear HTML ha sido complicado debido a la naturaleza flexible del marcado HTML; no todos los documentos HTML siguen las normas. Floki utiliza una dependencia llamada `mochiweb` para manejar esto. Otras alternativas en Elixir incluyen `mochiweb_html` y `html5ever`. Floki es distintivo por su sintaxis inspirada en jQuery, lo que facilita la búsqueda y extracción de datos en documentos HTML.

## Ver También:
- [Floki en Hex](https://hex.pm/packages/floki)
- [Documentación de Floki](https://hexdocs.pm/floki/Floki.html)
- [Github de Elixir](https://github.com/elixir-lang/elixir)
- [Elixir School](https://elixirschool.com/es/) - para recursos de aprendizaje en Español sobre Elixir.
