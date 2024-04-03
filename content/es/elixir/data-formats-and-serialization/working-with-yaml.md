---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:05.543201-07:00
description: "YAML, abreviatura de YAML Ain't Markup Language, es un est\xE1ndar de\
  \ serializaci\xF3n de datos legible por humanos com\xFAnmente utilizado para archivos\
  \ de\u2026"
lastmod: '2024-03-13T22:44:58.723559-06:00'
model: gpt-4-0125-preview
summary: "YAML, abreviatura de YAML Ain't Markup Language, es un est\xE1ndar de serializaci\xF3\
  n de datos legible por humanos com\xFAnmente utilizado para archivos de configuraci\xF3\
  n e intercambio de datos entre lenguajes con diferentes estructuras de datos."
title: Trabajando con YAML
weight: 41
---

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
