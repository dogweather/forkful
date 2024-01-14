---
title:    "Elixir: Encontrar la longitud de una cadena"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Por qué

 En la programación, es común tener la necesidad de encontrar la longitud de una cadena de caracteres. Ya sea para validar la entrada de usuario o para manipular datos, saber cuántos caracteres contiene una cadena es una habilidad importante para todo programador.

## Cómo hacerlo

En Elixir, podemos utilizar la función `String.length/1` para encontrar la longitud de una cadena. Esta función toma como argumento la cadena a la que se le quiere encontrar su longitud y devuelve un número entero.

```Elixir
iex> String.length("Hola mundo")
10
```

En este ejemplo, estamos utilizando la función `String.length/1` para encontrar la longitud de la cadena "Hola mundo" y el resultado es 10, ya que esa cadena contiene 10 caracteres.

También podemos utilizar la función `length/1` en lugar de `String.length/1`. Ambas funciones hacen lo mismo, pero la primera es una función genérica y puede ser utilizada para encontrar la longitud de cualquier tipo de dato, no solo cadenas.

```Elixir
iex> length("Elixir")
6
```

También podemos utilizar la función `byte_size/1` para encontrar la longitud de una cadena en bytes. Esta función cuenta el número de bytes necesarios para almacenar la cadena.

```Elixir
iex> byte_size("¡Hola mundo!")
13
```

Es importante tener en cuenta que en Elixir, las cadenas son listas de caracteres, por lo que también podemos utilizar la función `Enum.count/1` para encontrar la longitud de una cadena.

```Elixir
iex> Enum.count("¡Hola mundo!")
11
```

## Profundizando

Ahora que sabemos cómo encontrar la longitud de una cadena en Elixir, es importante mencionar que esta función también puede ser utilizada en múltiples cadenas o listas de caracteres. Si pasamos una lista de cadenas como argumento, la función `String.length/1` devolverá una lista con la longitud de cada cadena.

```Elixir
iex> String.length(["¡Hola", "mundo!"])
[5, 6]
```

También podemos utilizar la macro `__ENV__.file` para obtener el nombre y la ubicación del archivo en el que estamos trabajando en Elixir.

```Elixir
iex> String.length(__ENV__.file)
24
```

Otra forma interesante de utilizar la función `String.length/1` es pasarle una expresión regular como argumento y devolverá la longitud de la cadena que coincide con la expresión regular.

```Elixir
iex> String.length(~r/Hello World/)
11
```

## Ver también

- Documentación oficial de Elixir sobre `String.length/1`: https://hexdocs.pm/elixir/String.html#length/1
- Documentación oficial de Elixir sobre `Enum.count/1`: https://hexdocs.pm/elixir/Enum.html#count/1
- Documentación oficial de Elixir sobre `byte_size/1`: https://hexdocs.pm/elixir/String.html#byte_size/1