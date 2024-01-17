---
title:                "Generación de números aleatorios"
html_title:           "Elixir: Generación de números aleatorios"
simple_title:         "Generación de números aleatorios"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/generating-random-numbers.md"
---

{{< edit_this_page >}}

##¿Qué & Por qué?
Generar números aleatorios es un concepto importante en programación que se refiere a la creación de valores numéricos al azar. Los programadores utilizan este método para simular situaciones de incertidumbre o para realizar pruebas en su código.

##Cómo:
**Elixir** cuenta con una función incorporada llamada `:rand.uniform/1` que se encarga de generar números aleatorios. Esta función acepta un argumento que indica la cantidad de números aleatorios que deseamos obtener. A continuación se muestra un ejemplo de cómo utilizarla dentro del intérprete de **Elixir**:

```Elixir
iex> :rand.uniform(3)
1.2210634438228608
1.976798551157478
9.74060832931853
:ok
```

Como se puede observar, la función devuelve una lista con la cantidad de números aleatorios especificada.

##Profundizando:
El concepto de generar números aleatorios ha existido desde los inicios de la informática. Originalmente, se utilizaban fuentes de incertidumbre física, como lanzar una moneda o sacudir dados, para obtener números aleatorios. Sin embargo, con el avance de la tecnología, se desarrollaron algoritmos capaces de generar números aleatorios más precisos y eficientes.

En **Elixir**, además de la función `:rand.uniform/1`, también existen otras funciones que permiten generar diferentes tipos de números aleatorios, como por ejemplo `:rand.uniform/2` que se encarga de generar números dentro de un rango específico. 

##Ver también:
Puedes encontrar más información sobre la generación de números aleatorios en la documentación oficial de **Elixir**: https://hexdocs.pm/elixir/Random.html