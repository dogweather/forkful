---
title:                "Elixir: Uniendo cadenas"
simple_title:         "Uniendo cadenas"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/concatenating-strings.md"
---

{{< edit_this_page >}}

# Por qué unir cadenas de texto en Elixir

Unir o concatenar cadenas de texto es una tarea común en cualquier lenguaje de programación. En Elixir, es un proceso sencillo y eficiente que nos permite manejar y manipular cadenas de manera más dinámica.

## Cómo hacerlo

Para unir cadenas en Elixir, utilizamos el operador de concatenación `<>` entre los dos strings que queremos unir. Por ejemplo:

```Elixir
nombre = "Juan"
apellido = "Pérez"
nombre_completo = nombre <> " " <> apellido
```

En este ejemplo, utilizamos el operador `<>` para unir las variables `nombre` y `apellido` con un espacio en medio. El resultado final será la variable `nombre_completo` con el valor "Juan Pérez".

Otra forma de concatenar cadenas en Elixir es utilizando la función `String.concat/2`, que acepta una lista de cadenas y las une en una sola. Por ejemplo:

```Elixir
frase = String.concat(["Elixir", "es", "un", "lenguaje", "fácil", "y", "poderoso"])
```

El resultado de este ejemplo será la variable `frase` con el valor "Elixir es un lenguaje fácil y poderoso".

## Profundizando en la concatenación de cadenas

Es importante tener en cuenta que en Elixir, las cadenas de texto son inmutables, lo que significa que no se pueden modificar directamente. Por lo tanto, al unir cadenas estamos creando una nueva cadena en lugar de modificar la existente.

Además, es importante mencionar que también podemos unir cadenas con valores numéricos, ya que Elixir es un lenguaje dinámico. Por ejemplo:

```Elixir
numero = 5
resultado = "El número es" <> numero
```

En este caso, el resultado final será la cadena "El número es 5", ya que el operador `<>` también funciona con diferentes tipos de datos.

# Ver también

- [Documentación oficial de Elixir sobre strings](https://hexdocs.pm/elixir/String.html)
- [Ejercicios prácticos con cadenas de texto en Elixir](https://medium.com/@andreamiranda/5-ejercicios-practicos-con-cadenas-de-texto-en-elixir-ff66269673ae)
- [Artículo sobre los operadores en Elixir](https://medium.com/@lucasvmiguel/entendiendo-los-operadores-en-elixir-b66a3947a125)