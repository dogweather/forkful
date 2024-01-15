---
title:                "Utilizando expresiones regulares"
html_title:           "Elixir: Utilizando expresiones regulares"
simple_title:         "Utilizando expresiones regulares"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/using-regular-expressions.md"
---

{{< edit_this_page >}}

##Por qué

Si eres un programador de Elixir, es probable que ya hayas oído hablar de las expresiones regulares. Estas poderosas herramientas te permiten buscar y manipular cadenas de texto de manera eficiente. En este artículo, te explicaremos por qué deberías considerar utilizar expresiones regulares en tus proyectos de Elixir.

##Cómo

Las expresiones regulares en Elixir se definen utilizando el operador `~r` seguido de una cadena entre comillas `/`. Por ejemplo:

```Elixir
~r/hola/  # Esto buscará la palabra "hola" en una cadena de texto
```

Las expresiones regulares también admiten ciertos caracteres especiales para buscar patrones específicos:

- `.` coincidirá con cualquier carácter
- `+` coincidirá con una o más repeticiones del carácter anterior
- `*` coincidirá con cero o más repeticiones del carácter anterior
- `?` coincidirá con cero o una repetición del carácter anterior
- `\d` coincidirá con cualquier dígito
- `\w` coincidirá con cualquier carácter alfanumérico

Por ejemplo, la expresión regular `~r/^\w+@gmail.com/` coincidirá con cualquier dirección de correo electrónico que termine en "@gmail.com".

En el siguiente ejemplo, utilizaremos las expresiones regulares para reemplazar un texto específico en una cadena:

```Elixir
texto = "¡Hola a todos!"
~r/hola/ |> Regex.replace(texto, "adiós")  # Reemplazará "hola" con "adiós" en la cadena
```

##Profundizando

Las expresiones regulares pueden parecer intimidantes al principio, pero una vez que entiendas los patrones básicos y cómo utilizarlos, pueden ser una herramienta muy útil en tu caja de herramientas de programación. Algunos casos de uso comunes incluyen validar entradas de usuario, filtrar y extraer datos de grandes cantidades de texto y formatear cadenas complejas.

Sin embargo, también es importante tener en cuenta que las expresiones regulares no son la solución adecuada para todas las tareas. Si el texto que estás tratando de manipular es demasiado complejo o contiene patrones impredecibles, es posible que sea mejor utilizar otros métodos de manipulación de cadenas en lugar de expresiones regulares.

##Ver también

- [Documentación de Elixir sobre expresiones regulares](https://hexdocs.pm/elixir/Regex.html)
- [Tutorial de expresiones regulares para Elixir](https://www.tutorialspoint.com/elixir/elixir_regular_expressions.htm)
- [Introducción a las expresiones regulares en Elixir](https://dev.to/erjan_gavalji/introduction-to-regular-expressions-in-elixir-fh4)