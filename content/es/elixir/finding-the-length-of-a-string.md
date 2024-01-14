---
title:                "Elixir: Encontrando la longitud de una cadena"
simple_title:         "Encontrando la longitud de una cadena"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## ¿Por qué?

La longitud de una cadena es una tarea básica pero esencial en cualquier lenguaje de programación. Conocer la longitud de una cadena puede ser útil en varias situaciones, como validar la entrada del usuario o manipular datos en listas.

## Cómo hacerlo

Para encontrar la longitud de una cadena en Elixir, podemos utilizar la función `String.length/1`. Esta función toma como argumento una cadena y devuelve su longitud como un número entero. Veamos un ejemplo:

```elixir
nombre = "Juan"
String.length(nombre)
# output: 4
```

Como podemos ver, la función devuelve el número de caracteres en la cadena, incluyendo espacios y signos de puntuación.

También podemos utilizar la función `byte_size/1` para encontrar la cantidad de bytes que ocupa una cadena en la memoria. Esto puede ser útil si trabajamos con cadenas que contienen caracteres unicode, ya que cada unicode ocupa más de un byte en la memoria. Veamos un ejemplo:

```elixir
cadena = "Höla"
String.length(cadena)
# output: 4
byte_size(cadena)
# output: 5
```

Podemos utilizar estas funciones en conjunto con otras funciones de manipulación de cadenas para realizar tareas más complejas.

## Profundizando

La función `String.length/1` está definida en el módulo `String`, que a su vez utiliza una función interna llamada `length/1` que hace el recorrido y contabiliza los caracteres de la cadena. 

En términos de rendimiento, es importante tener en cuenta que `String.length/1` tiene una complejidad de tiempo O(n), es decir, su tiempo de ejecución es proporcional al tamaño de la cadena. Por lo tanto, es recomendable no utilizar esta función en cadenas muy largas ya que puede afectar el rendimiento de nuestra aplicación.

## Ver también

- Documentación oficial de Elixir sobre [`String.length/1`](https://hexdocs.pm/elixir/String.html#length/1)
- Ejemplos adicionales de uso de [`String.length/1`](https://www.tutorialspoint.com/elixir/elixir_string.htm)