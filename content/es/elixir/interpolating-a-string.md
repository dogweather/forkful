---
title:                "Interpolación de una cadena"
html_title:           "Elixir: Interpolación de una cadena"
simple_title:         "Interpolación de una cadena"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/interpolating-a-string.md"
---

{{< edit_this_page >}}

# ¿Qué y por qué?
La interpolación de cadenas es una técnica utilizada por programadores para insertar valores en una cadena de texto. Esto permite una mayor flexibilidad y dinamismo en la creación de cadenas de texto en un programa. Además, ayuda a evitar la concatenación de cadenas largas y tediosas.

# ¿Cómo hacerlo?
```Elixir
# Usando la sintaxis #{valor} dentro de una cadena de texto
nombre = "Juan"
edad = 25
"Hola, mi nombre es #{nombre} y tengo #{edad} años."
# Output: "Hola, mi nombre es Juan y tengo 25 años."
```

```Elixir
# También se pueden interpolar otras expresiones
numero1 = 5
numero2 = 10
"La suma de #{numero1} y #{numero2} es #{numero1 + numero2}."
# Output: "La suma de 5 y 10 es 15."
```

# Profundizando
La interpolación de cadenas se popularizó con la programación orientada a objetos en el lenguaje Smalltalk en la década de 1970. Actualmente, muchos lenguajes de programación modernos tienen soporte para esta técnica, como Ruby, Python y por supuesto, Elixir.

Como alternativas a la interpolación de cadenas, algunos lenguajes utilizan el operador de concatenación (+) o proporcionan funciones específicas para la creación de cadenas de texto. Sin embargo, la interpolación de cadenas sigue siendo una opción popular debido a su simplicidad y facilidad de uso.

En Elixir, la interpolación de cadenas es realizada por la macro "sigil", que permite una mayor integración con el resto del lenguaje y la posibilidad de utilizar módulos personalizados para el formateo de cadenas.

# Ver también
- Documentación oficial de Elixir sobre interpolación de cadenas: https://hexdocs.pm/elixir/stable/String.html#sigil
- Ejemplo de uso de interpolación de cadenas en Elixir: https://www.tutorialspoint.com/elixir/elixir_strings.htm