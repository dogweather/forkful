---
title:                "Elixir: Convirtiendo una cadena a minúsculas"
simple_title:         "Convirtiendo una cadena a minúsculas"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por qué

En la programación de Elixir, a menudo es necesario manipular y transformar cadenas de caracteres. Una de las tareas más comunes es convertir una cadena a minúsculas. Esto puede ser útil para comparar cadenas de manera más precisa o para formatear la salida de datos.

## Cómo hacerlo

Para convertir una cadena a minúsculas en Elixir, podemos utilizar la función `String.downcase/1`. Aquí hay un ejemplo de cómo usar esta función:

```Elixir
String.downcase("HELLO WORLD") # salida: "hello world"
```

En este ejemplo, pasamos una cadena en mayúsculas a la función `String.downcase/1` y recibimos una cadena en minúsculas como salida.

Podemos usar esta función para transformar una cadena en cualquier idioma, ya que Elixir es compatible con Unicode. Por ejemplo, si queremos convertir una cadena en español a minúsculas, también funcionará correctamente:

```Elixir
String.downcase("HOLA MUNDO") # salida: "hola mundo"
```

## Profundizando

Detrás de escena, la función `String.downcase/1` utiliza la función `String.downcase/2` con un segundo argumento de `:default`. Este argumento es un atajo para especificar la tabla de conversión de mayúsculas y minúsculas por defecto. Si quieres más control sobre la conversión, también puedes utilizar otras opciones, como `:en` o `:es` para especificar idiomas diferentes.

Podemos ver cómo se utiliza `:default` en el módulo `String` de Elixir:

```Elixir
@conversion_tables %{
  # ... otros idiomas
  :default => %{
    0x01c4 => 0x01c6,
    0x01c5 => 0x01c6,
    # ... otras conversiones
    },
    ...
  }
```

Esto significa que cuando especificamos `:default` como un argumento en `String.downcase/2`, se aplicará la tabla de conversión correspondiente a nuestro idioma. Si necesitamos diferentes conversiones, por ejemplo, para un idioma que no está soportado por defecto, podemos definir nuestras propias tablas de conversión de mayúsculas y minúsculas.

## Ver también
- [Documentación de Elixir sobre la función `String.downcase/1`](https://hexdocs.pm/elixir/String.html#downcase/1)
- [Guía de Elixir para trabajar con cadenas](https://elixir-lang.org/getting-started/string-operations.html)
- [Ejemplos de uso de `String.downcase/1` en la práctica](https://stackoverflow.com/questions/46934515/simply-downcasing-an-atom-to-lowercase-in-elixir)