---
aliases:
- /es/elixir/working-with-toml/
date: 2024-01-26 04:20:42.886588-07:00
description: "Trabajar con TOML significa analizar y generar datos de TOML (Tom's\
  \ Obvious, Minimal Language) usando Elixir. Los programadores lo usan para manejar\u2026"
lastmod: 2024-02-18 23:09:09.682504
model: gpt-4-0125-preview
summary: "Trabajar con TOML significa analizar y generar datos de TOML (Tom's Obvious,\
  \ Minimal Language) usando Elixir. Los programadores lo usan para manejar\u2026"
title: Trabajando con TOML
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
Trabajar con TOML significa analizar y generar datos de TOML (Tom's Obvious, Minimal Language) usando Elixir. Los programadores lo usan para manejar archivos de configuración porque TOML es legible, fácil de analizar y se mapea bien a una estructura de datos tipo hash.

## Cómo hacerlo:
Primero, añade un analizador de TOML a tus dependencias mix. Este ejemplo utiliza `toml-elixir`:

```elixir
def deps do
  [
    {:toml_elixir, "~> 2.0"}
  ]
end
```

Lee un archivo TOML:

```elixir
{:ok, toml_data} = File.read("config.toml")
{:ok, parsed_data} = TomlElixir.parse(toml_data)
```

Para convertir datos de Elixir a TOML:

```elixir
data = %{title: "Ejemplo TOML", owner: %{name: "Tom Preston-Werner"}}
toml_string = TomlElixir.encode(data)
```

Muestra de salida:

```elixir
"title = \"Ejemplo TOML\"\n\n[owner]\nname = \"Tom Preston-Werner\"\n"
```

## Profundización
TOML fue creado por Tom Preston-Werner, cofundador de GitHub, para su uso en archivos de configuración. Está diseñado para ser más sencillo que XML y más conciso que YAML, manteniendo la consistencia.

Las alternativas incluyen archivos JSON, YAML y INI, cada uno con sus compensaciones en legibilidad humana y compatibilidad de estructura de datos. TOML destaca en representar claramente datos tabulares y la agrupación anidada de datos.

En Elixir, el manejo de TOML depende de bibliotecas de codificación y decodificación, las cuales transforman cadenas TOML en mapas de Elixir y viceversa. El análisis funciona haciendo coincidir las reglas de sintaxis de TOML y convirtiéndolas en tipos de datos de Elixir. La codificación hace lo opuesto, mapeando los tipos de datos de Elixir de vuelta a la sintaxis TOML válida.

## Ver También
- Lenguaje TOML: https://toml.io/en/
- Repositorio de GitHub de `toml-elixir`: https://github.com/bitwalker/toml-elixir
- Detalles del paquete Hex para `toml-elixir`: https://hex.pm/packages/toml_elixir
