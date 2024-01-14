---
title:                "Elixir: Imprimiendo salida de depuración"
programming_language: "Elixir"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por qué

A veces, cuando estamos desarrollando un programa en Elixir, nos encontramos con errores o problemas que no parecen fáciles de resolver. En estos casos, imprimir resultados de depuración puede ser una herramienta útil para entender qué está sucediendo en nuestro código y cómo solucionarlo.

## Cómo

Para imprimir resultados de depuración en Elixir, podemos utilizar la función `IO.inspect/2`. Esta función nos permite imprimir cualquier valor o variable en la consola. Veamos un ejemplo:

```Elixir
data = [1, 2, 3]
IO.inspect(data)
```

La salida en la consola será:

```
[1, 2, 3]
```

Podemos incluso utilizar esta función dentro de una expresión Elixir, como en el siguiente ejemplo:

```Elixir
num = 5
IO.inspect("El número es #{num}")
```

La salida en la consola será:

```
El número es 5
```

## Profundizando

Además de imprimir resultados de depuración simples, la función `IO.inspect/2` también acepta opciones adicionales que pueden ser útiles en diferentes situaciones. Por ejemplo, podemos utilizar la opción `:label` para proporcionar un nombre o descripción al resultado impreso:

```Elixir
num = 10
IO.inspect(num, label: "Número")
```

La salida en la consola será:

```
Número: 10
```

Otra opción interesante es `:pretty`, que nos permite imprimir resultados en un formato más legible. Por ejemplo, si tenemos un mapa con varios valores, podemos utilizar esta opción para imprimirlo en formato JSON:

```Elixir
data = %{"nombre" => "Juan", "edad" => 30}
IO.inspect(data, pretty: true)
```

La salida en consola será:

```
%{
  "nombre" => "Juan",
  "edad" => 30
}
```

Puedes encontrar más opciones y ejemplos en la documentación oficial de Elixir para `IO.inspect/2`.

## Ver también

- [Documentación oficial de Elixir para la función IO.inspect/2](https://hexdocs.pm/elixir/IO.html#inspect/2)
- [Tutorial de Elixir en español](https://elixircourse.com/es/)

¡Espero que esta breve introducción sobre imprimir resultados de depuración en Elixir te haya sido útil! ¡Sigue aprendiendo y explorando para convertirte en un experto en este lenguaje de programación funcional!