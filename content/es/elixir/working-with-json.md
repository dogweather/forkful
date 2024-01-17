---
title:                "Trabajando con json"
html_title:           "Elixir: Trabajando con json"
simple_title:         "Trabajando con json"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/working-with-json.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Trabajar con JSON es una forma común en la que los programadores almacenan y transfieren datos en sus aplicaciones. JSON, o Javascript Object Notation, es un formato de intercambio de datos basado en texto que es fácil de leer y escribir para los humanos y fácil de interpretar para las computadoras. Los programadores usan JSON para representar datos estructurados como objetos y arrays, lo que hace que sea más fácil compartir información entre aplicaciones.

## Cómo:
Para trabajar con JSON en Elixir, necesitamos utilizar el módulo `Jason`, que viene incluido en la biblioteca estándar de Elixir. Para convertir un término en una cadena JSON, podemos usar la función `encode!/1` del módulo `Jason`.

```Elixir
iex> data = %{name: "Juan", age: 25}
%{age: 25, name: "Juan"}
iex> Jason.encode!(data)
"{\"name\":\"Juan\",\"age\":25}"
```

Para convertir una cadena JSON en términos de Elixir, podemos usar la función `decode!/1`. Si queremos acceder a un valor específico en el JSON, podemos utilizar los operadores de acceso como lo haríamos con cualquier otro término de Elixir.

```Elixir
iex> json = "{\"name\":\"Juan\",\"age\":25}"
"{\"name\":\"Juan\",\"age\":25}"
iex> Jason.decode!(json)
%{age: 25, name: "Juan"}
iex> Jason.decode!(json)["name"]
"Juan"
```

## Inmersión profunda:
JSON fue creado originalmente por Douglas Crockford en 2001 y se ha convertido en un formato de intercambio de datos muy popular. Otras alternativas para el intercambio de datos incluyen XML y YAML. Elixir hace que trabajar con JSON sea muy sencillo gracias a la inclusión del módulo `Jason` en su biblioteca estándar. Internamente, `Jason` utiliza el módulo `Poison` para realizar la codificación y decodificación. También tiene funciones de validación y opciones de configuración para personalizar el proceso de codificación y decodificación de JSON.

## Ver también:
- Documentación oficial de Jason: https://hexdocs.pm/jason/index.html
- Tutorial para trabajar con JSON en Elixir: https://www.culttt.com/2016/04/04/json-elixir/