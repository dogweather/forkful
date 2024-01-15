---
title:                "Trabajando con yaml"
html_title:           "Elixir: Trabajando con yaml"
simple_title:         "Trabajando con yaml"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/working-with-yaml.md"
---

{{< edit_this_page >}}

## Por qué

¿Te sientes abrumado por la cantidad de configuraciones que tienes que hacer para tu proyecto? ¿Deseas tener una forma más sencilla y legible de manejar datos estructurados? ¡Entonces YAML es la solución que estabas buscando! YAML es un lenguaje de serialización de datos que proporciona una sintaxis fácil de leer y escribir, lo que lo hace perfecto para manejar configuraciones y datos estructurados en tus proyectos de Elixir.

## Cómo hacerlo

La primera cosa que debes hacer es agregar la dependencia de YAML a tu proyecto de Elixir. Puedes hacer esto agregando `{:yaml_elixir, "~> 2.0"}` a tu archivo `mix.exs` en la sección de dependencias y luego ejecutando `mix deps.get` en tu terminal para instalar la dependencia.

Una vez que tengas la dependencia instalada, puedes usar el módulo `YAML` para convertir datos estructurados en un formato YAML, y viceversa. Aquí hay un ejemplo para convertir un mapa a un formato YAML:

```Elixir
data = %{name: "María", age: 25, hobbies: ["leer", "pasear en bicicleta"]}

yaml = YAML.dump(data)
```

Este código producirá la siguiente salida:

```YAML
name: María
age: 25
hobbies:
  - leer
  - pasear en bicicleta
```

También puedes convertir una cadena de YAML en un mapa utilizando la función `YAML.load/1`. Por ejemplo:

```Elixir
yaml = """
name: Juan
age: 30
hobbies:
  - jugar videojuegos
  - escuchar música
"""

data = YAML.load(yaml)
```

Este código producirá el siguiente mapa:

```Elixir
%{age: 30, hobbies: ["jugar videojuegos", "escuchar música"], name: "Juan"}
```

## Profundizando

Además de simplemente convertir datos a y desde formato YAML, el módulo `YAML` también te permite trabajar con archivos YAML. Por ejemplo, puedes cargar un archivo YAML directamente en un mapa utilizando la función `YAML.load_file/1`. Aquí hay un ejemplo:

```Elixir
data = YAML.load_file("./ejemplo.yaml")
```

También puedes utilizar la función `YAML.dump_file/2` para escribir un mapa en un archivo YAML. Aquí hay un ejemplo:

```Elixir
data = %{nombre: "Ana", edad: 28, profesión: "programadora"}

YAML.dump_file(data, "./info.yaml")
```

Este código escribirá el mapa en un archivo `info.yaml` con el siguiente contenido:

```YAML
nombre: Ana
edad: 28
profesión: programadora
```

## Ver también

- [Documentación oficial de YAML Elixir](https://hexdocs.pm/yaml/Elixir.html)
- [Código fuente de YAML Elixir](https://github.com/KamilLelonek/yaml-elixir)
- [Tutorial de YAML para Elixir (en inglés)](https://www.jungledisk.com/blog/2018/02/01/getting-started-yaml-elixir-tutorial/)