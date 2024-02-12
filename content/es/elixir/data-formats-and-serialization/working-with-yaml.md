---
title:                "Trabajando con YAML"
aliases: - /es/elixir/working-with-yaml.md
date:                  2024-02-03T19:25:05.543201-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trabajando con YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

YAML, abreviatura de YAML Ain't Markup Language, es un estándar de serialización de datos legible por humanos comúnmente utilizado para archivos de configuración e intercambio de datos entre lenguajes con diferentes estructuras de datos. Los programadores lo utilizan debido a su simplicidad y su capacidad para representar fácilmente datos jerárquicos complejos.

## Cómo hacerlo:

Elixir no incluye soporte incorporado para YAML. Sin embargo, puedes utilizar bibliotecas de terceros tales como `yamerl` o `yaml_elixir` para trabajar con YAML. Aquí, nos centraremos en `yaml_elixir` por su facilidad de uso y características completas.

Primero, agrega `yaml_elixir` a tus dependencias en mix.exs:

```elixir
defp deps do
  [
    {:yaml_elixir, "~> 2.9"}
  ]
end
```

Luego, ejecuta `mix deps.get` para obtener la nueva dependencia.

### Leyendo YAML

Dados un simple archivo YAML, `config.yaml`, que se ve así:

```yaml
database:
  adapter: postgres
  username: user
  password: pass
```

Puedes leer este archivo YAML y convertirlo en un mapa de Elixir de la siguiente manera:

```elixir
defmodule Config do
  def read do
    {:ok, content} = YamlElixir.read_from_file("config.yaml")
    content
  end
end

# Ejemplo de uso
Config.read()
# Salida: 
# %{
#   "database" => %{
#     "adapter" => "postgres",
#     "username" => "user",
#     "password" => "pass"
#   }
# }
```

### Escribiendo YAML

Para escribir un mapa de vuelta a un archivo YAML:

```elixir
defmodule ConfigWriter do
  def write do
    content = %{
      database: %{
        adapter: "mysql",
        username: "root",
        password: "s3cret"
      }
    }
    
    YamlElixir.write_to_file("new_config.yaml", content)
  end
end

# Ejemplo de uso
ConfigWriter.write()
# Esto creará o sobrescribirá `new_config.yaml` con el contenido especificado
```

Observa cómo `yaml_elixir` permite una traducción directa entre archivos YAML y estructuras de datos de Elixir, convirtiéndolo en una excelente opción para los programadores de Elixir que necesitan trabajar con datos YAML.
