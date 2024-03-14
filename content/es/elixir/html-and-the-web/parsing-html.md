---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:11:54.556806-07:00
description: "Parsear HTML en Elixir implica extraer informaci\xF3n de documentos\
  \ HTML. Los programadores hacen esto para interactuar program\xE1ticamente con p\xE1\
  ginas web,\u2026"
lastmod: '2024-03-13T22:44:58.700247-06:00'
model: gpt-4-0125-preview
summary: "Parsear HTML en Elixir implica extraer informaci\xF3n de documentos HTML.\
  \ Los programadores hacen esto para interactuar program\xE1ticamente con p\xE1ginas\
  \ web,\u2026"
title: Analizando HTML
---

{{< edit_this_page >}}

## Qué y Por Qué?

Parsear HTML en Elixir implica extraer información de documentos HTML. Los programadores hacen esto para interactuar programáticamente con páginas web, recopilar datos, o automatizar interacciones web, permitiendo que las aplicaciones entiendan y utilicen contenido web de manera dinámica.

## Cómo hacerlo:

Elixir, con su robusto modelo de concurrencia y paradigma de programación funcional, no incluye capacidades de parseo de HTML incorporadas. Sin embargo, puedes utilizar bibliotecas de terceros populares como `Floki` para este propósito. Floki hace que el parseo de HTML sea intuitivo y eficiente, aprovechando las características de coincidencia de patrones y canalización de Elixir.

Primero, añade Floki a tus dependencias en mix.exs:

```elixir
defp deps do
  [
    {:floki, "~> 0.31.0"}
  ]
end
```

Luego, ejecuta `mix deps.get` para instalar la nueva dependencia.

Ahora, vamos a parsear una cadena HTML simple para extraer datos. Buscaremos los títulos dentro de las etiquetas `<h1>`:

```elixir
html_content = """
<html>
  <body>
    <h1>Hola, Elixir!</h1>
    <h1>Otro Título</h1>
  </body>
</html>
"""

titles = html_content
         |> Floki.find("h1")
         |> Floki.text()

IO.inspect(titles)
```

**Salida de Muestra:**

```elixir
["Hola, Elixir!", "Otro Título"]
```

Para profundizar, digamos que quieres extraer enlaces (etiquetas `<a>`) junto con sus atributos href. Aquí te mostramos cómo puedes lograrlo:

```elixir
html_content = """
<html>
  <body>
    <a href="https://elixir-lang.org/">Sitio Oficial de Elixir</a>
    <a href="https://hexdocs.pm/">HexDocs</a>
  </body>
</html>
"""

links = html_content
        |> Floki.find("a")
        |> Enum.map(fn({_, attrs, [text]}) -> {text, List.keyfind(attrs, "href", 0)} end)
        
IO.inspect(links)
```

**Salida de Muestra:**

```elixir
[{"Sitio Oficial de Elixir", {"href", "https://elixir-lang.org/"}}, {"HexDocs", {"href", "https://hexdocs.pm/"}}]
```

Este enfoque te permite navegar y parsear documentos HTML de manera eficiente, haciendo que las tareas de extracción y manipulación de datos web sean sencillas en aplicaciones Elixir.
