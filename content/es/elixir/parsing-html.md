---
title:                "Analizando html"
html_title:           "Elixir: Analizando html"
simple_title:         "Analizando html"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/parsing-html.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

La lectura y análisis de HTML es un proceso importante para los programadores que desarrollan aplicaciones web. Esto implica tomar un código HTML y convertirlo en una estructura de datos que pueda ser procesada por una aplicación. La razón por la que los programadores lo hacen es para poder extraer información específica de un sitio web o para manipular el contenido de una manera determinada.

## Cómo hacerlo:

La librería de Elixir `Floki` proporciona una manera simple y poderosa de analizar HTML. Primero, instalamos la librería en nuestro proyecto y luego la importamos en nuestro módulo:

```Elixir
defp deps do
  [{:floki, "~> 0.26"}]
end
```

```Elixir
import Floki
```

A continuación, podemos usar la función `html_to_iodata/1` para convertir el código HTML en una estructura de datos que podemos manipular:

```Elixir
html = "<h1>Hello World</h1>"
parsed = html |> Floki.parse |> Floki.html_to_iodata
IO.puts(inspect(parsed))
```

Esto nos dará el siguiente resultado:

```Elixir
{:html, [], [{:h1, [], ["Hello World"]}]}
```

Ahora podemos acceder a la información que necesitemos y hacer cualquier manipulación que deseemos en la estructura de datos.

## Profundizando:

La lectura y análisis de HTML ha sido una práctica común en el desarrollo web desde los inicios de la web. Antes de herramientas como `Floki`, los programadores tenían que escribir su propio código para manipular el HTML en bruto. Aunque todavía existen otras opciones para analizar HTML como `mechanize` o `hpricot`, `Floki` se destaca por su uso de patrones de búsqueda y su integración con el lenguaje funcional de Elixir.

## Vea también:

- Página de [Elixir](https://elixir-lang.org/) para obtener más información sobre el lenguaje de programación.
- [Documentación de Floki](https://hexdocs.pm/floki/Floki.html) para una referencia detallada de la librería.
- [Elixir School](https://elixirschool.com/en/) para aprender más sobre Elixir y otros conceptos de programación funcional.