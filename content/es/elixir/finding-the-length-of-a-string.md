---
title:                "Elixir: Encontrando la longitud de una cadena"
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Por qué

En programación, a menudo necesitamos saber la longitud de una cadena de texto, ya sea para validar la entrada del usuario, procesar datos o realizar operaciones específicas. Aunque puede parecer una tarea sencilla, saber cómo obtener la longitud de una cadena en Elixir es una habilidad fundamental para cualquier programador.

## Cómo hacerlo

Para obtener la longitud de una cadena en Elixir, podemos utilizar la función `String.length/1`. Esta función toma una cadena como argumento y devuelve su longitud como un número entero. Veamos un ejemplo:

```elixir
cadena = "¡Hola mundo!"
longitud = String.length(cadena)
```

En este código, hemos asignado la cadena "¡Hola mundo!" a la variable `cadena` y luego utilizamos la función `String.length/1` para obtener su longitud. La variable `longitud` ahora contiene el valor `12`, que es el número de caracteres en la cadena.

## Inmersión profunda

Puede que te preguntes cómo funciona exactamente la función `String.length/1`. Lo que sucede es que Elixir convierte la cadena en una lista de caracteres y luego cuenta el número de elementos en esa lista. Por lo tanto, si utilizas `String.length/1` en una cadena que contiene caracteres especiales o emojis, aún obtendrás la cantidad correcta de caracteres.

Además, si tienes una cadena vacía, la función `String.length/1` devolverá `0`, lo que es importante tener en cuenta al manipular datos en Elixir.

## Ver también

- [Documentación oficial de Elixir sobre String.length/1](https://hexdocs.pm/elixir/String.html#length/1)
- [Tutorial de Elixir: Trabajando con cadenas de texto](https://elixirschool.com/es/lessons/basics/basics/#trabajando-con-cadenas-de-texto)
- [Ejemplos interactivos: Obtener la longitud de una cadena en Elixir](https://rextester.com/edit/JLAZ21223)