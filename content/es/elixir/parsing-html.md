---
title:                "Análisis sintáctico de html"
html_title:           "Ruby: Análisis sintáctico de html"
simple_title:         "Análisis sintáctico de html"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/parsing-html.md"
---

{{< edit_this_page >}}

#  Parsing HTML con Elixir  


## ¿Qué y Por qué?

El Análisis (parsing) de HTML implica interpretar una string de HTML y trabajarla para su manipulación y extracción de datos. Los programadores lo hacen para consumir contenido web programáticamente y automatizar tareas en línea.

## Cómo hacerlo:

En Elixir, el paquete Floki es una excelente opción para analizar HTML. Primero, instalemos la dependencia Floki agregándola en ```mix.exs```.

```Elixir
defp deps do
  [
    {:floki, "~> 0.30.0"}
  ]
end
```

No olvidemos ejecutar ```mix deps.get``` para instalar.

Ahora, usemos Floki para analizar un simple documento HTML.

```Elixir
defmodule HtmlParser do
  def parse do
    html_string = ~s(<div><p>¡Hola, Mundo!</p></div>)
    parsed_html = Floki.parse_document(html_string)
    IO.inspect parsed_html
  end
end
```

Ejecuta la función ```parse``` y obtendrás una salida de la estructura de árbol almacenada como una tupla en Elixir.

## Inmersión Profunda

Históricamente, el análisis de HTML ha sido un desafío en cualquier lenguaje de programación debido a la naturaleza flexible del HTML, que es más una característica que un error. Así se hizo con la intención de que los navegadores pudieran interpretar HTML incluso si los desarrolladores cometieran errores.

Existen alternativas a Floki en Elixir como ```Meeseeks``` y ```Sweet_xml```, aunque Floki tiende a ser la opción popular dada su facilidad de uso y rendimiento.

Floki implementa un proceso de dos pasos en el análisis de HTML: primero, utiliza un analizador llamado Moebius para desglosar la string de HTML en tokens. Luego, estos tokens se envían a un proceso de 'árbol de construcción'.

## Ver También

Aquí hay algunos enlaces a fuentes relacionadas que pueden ser de interés:

[Floki en Hex](https://hex.pm/packages/floki)

[Documentación de Floki](https://hexdocs.pm/floki)

[Artículo comparativo: Floki vs Meeseeks](https://medium.com/@kenmazaika/elixir-html-parsing-with-floki-vs-meeseeks-27f67d22b17d)

¡Vamos a programar!