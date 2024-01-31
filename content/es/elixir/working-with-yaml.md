---
title:                "Trabajando con YAML"
date:                  2024-01-19
html_title:           "Arduino: Trabajando con YAML"
simple_title:         "Trabajando con YAML"

category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/working-with-yaml.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Trabajar con YAML significa manipular datos en el formato YAML ("YAML Ain't Markup Language"), común en configuración y transferencia de datos por su legibilidad humana. Programadores lo usan por su simplicidad y facilidad de uso en múltiples lenguajes de programación.

## Cómo hacerlo:

Elixir no incluye un parser de YAML en la biblioteca estándar, pero puedes usar la librería `yamerl` disponible en Hex. Primero, agrega la dependencia a tu `mix.exs`:

```elixir
defp deps do
  [
    {:yamerl, "~> 0.8"}
  ]
end
```

Luego puedes cargar y parsear YAML de esta manera:

```elixir
{:ok, yaml} = YamlElixir.read_from_file("config.yaml")
IO.inspect(yaml)
```

Si `config.yaml` contiene:

```
nombre: "Alejandro"
ocupacion: "Desarrollador"
```

La salida será un mapa de Elixir:

```elixir
%{"nombre" => "Alejandro", "ocupacion" => "Desarrollador"}
```

Para guardar datos en un archivo YAML:

```elixir
datos = %{"nombre" => "Alejandro", "ocupacion" => "Desarrollador"}
File.write!("config.yaml", YamlElixir.write_to_string(datos))
```

Esto crea o sobrescribe `config.yaml` con el contenido estructurado.

## Profundización

YAML se diseñó en 2001 para ser amigable a humanos y trabajable con lenguajes de scripting. Alternativas incluyen JSON y XML, pero YAML destaca en configuraciones por su claridad. La implementación en Elixir generalmente requiere librerías externas porque el idioma valora la concisión y deja ciertas funcionalidades a la comunidad.

## Vea También:

- YAML especificación: [https://yaml.org/spec/1.2/spec.html](https://yaml.org/spec/1.2/spec.html)
- Otros parsers de YAML en Elixir en Hex: [https://hex.pm](https://hex.pm) (buscar "YAML")
