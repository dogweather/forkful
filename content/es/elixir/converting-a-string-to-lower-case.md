---
title:    "Elixir: Convirtiendo una cadena a minúsculas"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Por qué

Convertir una cadena de texto a minúsculas puede ser útil en muchas situaciones de programación, como el procesamiento de datos de usuario o la comparación de cadenas.

## Cómo hacerlo

En Elixir, puedes usar la función `String.downcase/1` para convertir una cadena a minúsculas. Aquí hay un ejemplo de código:

``` elixir
string = "HOLA MUNDO"
lowercase_string = String.downcase(string)
IO.puts(lowercase_string)
```

La salida de este código será `hola mundo`.

También puedes usar una cadena interpolada para llamar a la función `downcase` directamente en la cadena original, como se muestra a continuación:

``` elixir
string = "HOLA MUNDO"
lowercase_string = "#{string.downcase}"
IO.puts(lowercase_string)
```

La salida será la misma que en el ejemplo anterior.

## Inmersión profunda

La función `String.downcase/1` utiliza reglas Unicode para convertir la cadena a minúsculas. Esto significa que algunos caracteres pueden cambiar a un caso diferente al esperado en ciertos idiomas. Por ejemplo, la letra "I" mayúscula en turco se convierte en "ı" minúscula en lugar de "i".

También es importante tener en cuenta que la función devuelve una nueva cadena, por lo que la cadena original no se modificará. Si deseas modificar la cadena original, puedes usar la función `String.downcase!/1`.

## Ver también

- [Documentación oficial de Elixir para String.downcase/1](https://hexdocs.pm/elixir/String.html#downcase/1)
- [Artículo sobre cómo manejar cadenas en Elixir](https://tedic.org/manejo-de-cadenas-en-elixir.html)