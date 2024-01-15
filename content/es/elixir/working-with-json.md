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

## ¿Por qué trabajar con JSON en Elixir?

Trabajar con JSON (JavaScript Object Notation) puede ser muy útil en el desarrollo de aplicaciones en Elixir. JSON es un formato de intercambio de datos ligero y fácil de leer que se ha convertido en el estándar para la comunicación entre aplicaciones web. Con Elixir, podemos manejar fácilmente los objetos JSON y su manipulación nos permitirá enviar y recibir datos de forma eficiente.

## Cómo hacerlo

Para trabajar con JSON en Elixir, es necesario primero convertir los datos a una estructura compatible con Elixir. Esto se puede hacer utilizando la biblioteca Jason, que es una de las más populares para trabajar con JSON en Elixir.

Primero, es necesario agregar la dependencia de Jason en nuestro archivo `mix.exs`:

```Elixir
defp deps do
  [{:jason, "~> 1.2"}]
end
```

Luego, podemos utilizar la función `decode/1` para convertir una cadena JSON en una estructura de datos en Elixir. Por ejemplo, si tenemos el siguiente JSON:

```Elixir
json = """
  {
    "name": "John",
    "age": 25,
    "hobbies": ["running", "reading"]
  }
"""
```

Podemos convertirlo a una estructura de Elixir de la siguiente manera:

```Elixir
Jason.decode(json)
```

Esto nos devolverá un mapa de Elixir con las claves y valores correspondientes:

```Elixir
%{"name" => "John", "age" => 25, "hobbies" => ["running", "reading"]}
```

También podemos utilizar la función `encode/1` para convertir una estructura de datos en Elixir a un formato de cadena JSON:

```Elixir
Jason.encode(%{"name" => "John", "age" => 25, "hobbies" => ["running", "reading"]})
```

Esto nos devolverá la cadena JSON correspondiente:

```Elixir
"{\"name\":\"John\",\"age\":25,\"hobbies\":[\"running\",\"reading\"]}"
```

## Profundizando

Además de la conversión básica de datos entre JSON y Elixir, la biblioteca Jason también ofrece otras funcionalidades útiles para trabajar con JSON. Algunas de estas funciones son:

- La función `decode!/1` que funciona de la misma manera que `decode/1` pero levanta una excepción en caso de error.
- La función `encode!/1` que funciona de la misma manera que `encode/1` pero levanta una excepción en caso de error.
- La función `Encode.stream/2` que nos permite convertir una estructura de datos en una secuencia de codificación, lo que puede ser más eficiente para grandes cantidades de datos.
- La función `decode_file/2` que nos permite decodificar un archivo JSON directamente en una estructura de datos en Elixir.

Para obtener más información sobre estas y otras funciones de la biblioteca Jason, puedes consultar su [documentación](https://hexdocs.pm/jason/Jason.html).

## Ver también

- [Elixir Docs: Jason](https://hexdocs.pm/jason/Jason.html)
- [Learn Elixir: Working with JSON](https://www.youtube.com/watch?v=sHc-gKAhU60)
- [ElixirSchool: Working with JSON](https://elixirschool.com/lessons/basics/json/)