---
title:                "Interpolando una cadena de texto"
html_title:           "Haskell: Interpolando una cadena de texto"
simple_title:         "Interpolando una cadena de texto"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/interpolating-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

La interpolación de cadenas en Elixir permite incluir valores de variables dentro de cadenas. Es rápida, fácil de usar y facilita la legibilidad y claridad del código.

## ¿Cómo hacerlo?

En Elixir, se realiza la interpolación de cadenas utilizando el símbolo `#` seguido de llaves `{}`. Abajo, un ejemplo.

```elixir
nombre = "Carlos"
IO.puts "Hola, #{nombre}!"
```

La salida sería:

```elixir
Hola, Carlos!
```

Si lo que tienes es una expresión, también puedes interpolarla en una cadena. Veamos un ejemplo:

```elixir
x = 5 
y = 10
IO.puts "Suma: #{x + y}"
```

Lo que imprimirá:

```elixir
Suma: 15
```

## Excavando un poco más profundo

La interpolación de cadenas es una característica que viene de lenguajes de programación más antiguos como Perl, y ha sido adoptada por muchos otros lenguajes modernos por su comodidad y eficiencia.

En cuanto a alternativas, se podrían concatenar cadenas usando el operador `<>`, pero eso es más tedioso y genera un código menos legible.

Desde el punto de vista de la implementación, cuando interpolas una cadena, Elixir genera internamente una serie de concatenaciones. Sin embargo, este detalle está oculto para los desarrolladores para que se pueda mantener un código más limpio y fácil de entender.

## Ver también

Para más información:

[Documentación oficial de Elixir](https://elixir-lang.org/getting-started/basic-types.html#strings)

[Interpolación de cadenas en Elixir](https://elixirschool.com/es/lessons/basics/strings/)