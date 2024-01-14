---
title:                "Elixir: Analizando html"
simple_title:         "Analizando html"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/parsing-html.md"
---

{{< edit_this_page >}}

## Por qué
El análisis de HTML es crucial para cualquier persona interesada en desarrollar aplicaciones web en Elixir. No solo le permite extraer y manipular datos de una página web, sino que también puede ayudar a automatizar procesos y mejorar la eficiencia del desarrollo.

## Cómo hacerlo
Para realizar el análisis de HTML en Elixir, podemos utilizar la biblioteca Floki. Primero, debemos instalarla en nuestro proyecto agregando `{:floki, "~> 0.30"}` a la lista de dependencias en nuestro archivo `mix.exs`. Luego, podemos comenzar a realizar análisis de HTML utilizando la función `Floki.parse/1`. Por ejemplo, si queremos obtener el título de una página, podemos hacer lo siguiente:

```Elixir
html = "<html><head><title>Mi blog de Elixir</title></head></html>"
Floki.parse(html) |> Floki.find("title") |> Floki.text() # Devuelve "Mi blog de Elixir"
```

Del mismo modo, podemos utilizar `Floki.find/2` para encontrar un elemento específico en la página y `Floki.attribute/2` para obtener sus atributos. Para más ejemplos y opciones de búsqueda, consulta la documentación de Floki en [https://hexdocs.pm/floki/readme.html](https://hexdocs.pm/floki/readme.html).

## Profundizando
El análisis de HTML a menudo implica más de lo que se puede hacer con Floki. Por ejemplo, puede ser necesario recorrer múltiples páginas o realizar acciones en función de ciertas condiciones en la página. Para eso, se pueden utilizar otras bibliotecas como HTTPotion para obtener los datos de la página y Poison para analizar el código fuente HTML. También se pueden utilizar otras opciones de búsqueda avanzada en Floki, como `Floki.match/4` y `Floki.select/2`. Explora lo que estas bibliotecas tienen para ofrecer y encuentra la combinación perfecta para tus necesidades.

## Ver también
- [https://hexdocs.pm/floki/readme.html](https://hexdocs.pm/floki/readme.html)
- [https://github.com/edgurgel/httpotion](https://github.com/edgurgel/httpotion)
- [https://github.com/devinus/poison](https://github.com/devinus/poison)