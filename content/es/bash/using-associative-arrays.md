---
title:                "Uso de matrices asociativas"
date:                  2024-01-30T19:09:56.818043-07:00
model:                 gpt-4-0125-preview
simple_title:         "Uso de matrices asociativas"

tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Qué y Por Qué?

Los arreglos asociativos son como arreglos súper cargados que te permiten usar cadenas como índices en lugar de solo enteros. Los programadores los utilizan para estructuras de datos más complejas, facilitando el manejo de datos que no encajan ordenadamente en una lista secuencial.

## Cómo hacerlo:

Primero, declare un arreglo asociativo en Bash:

```Bash
declare -A my_array
```

Luego, puedes comenzar a poblarlo con valores, usando cadenas como claves:

```Bash
my_array["name"]="Linux Journal"
my_array["topic"]="Programación"
```

Para acceder a un elemento, usa su clave:

```Bash
echo ${my_array["name"]}  # Salida: Linux Journal
```

Iterar sobre claves y valores también es sencillo:

```Bash
for key in "${!my_array[@]}"; do
    echo "$key: ${my_array[$key]}"
done
```

La salida de muestra podría verse así:

```
name: Linux Journal
topic: Programación
```

Para agregar o modificar elementos, simplemente asigna un valor a una clave, de manera similar a como se pobló inicialmente:

```Bash
my_array["readers"]="Tú"
```

Y para eliminar un elemento, usa `unset`:

```Bash
unset my_array["topic"]
```

## Análisis Profundo

Los arreglos asociativos fueron introducidos en la versión 4.0 de Bash, lo que los hace una adición relativamente reciente al lenguaje. Antes de su introducción, manejar arreglos con índices no enteros era engorroso, a menudo requiriendo soluciones alternativas o herramientas externas como `awk` o `sed`.

Por debajo, Bash implementa los arreglos asociativos usando tablas hash. Esta implementación permite una búsqueda de clave eficiente, que permanece bastante constante independientemente del tamaño del arreglo, una característica crítica para el rendimiento en la ejecución de scripts.

Aunque los arreglos asociativos en Bash aportan mucha potencia y flexibilidad a los scripts de shell, vienen con su propio conjunto de limitaciones, como ser algo más torpes de manejar en comparación con los arreglos en lenguajes de más alto nivel como Python o JavaScript. Para tareas de manipulación de datos complejas, podría seguir valiendo la pena considerar herramientas o lenguajes externos más adecuados para el trabajo.

Sin embargo, para muchas tareas de scripting típicas, los arreglos asociativos proporcionan una herramienta valiosa en el conjunto de herramientas del programador de Bash, permitiendo scripts más legibles y mantenibles al permitir el uso de claves de cadena significativas en lugar de índices numéricos.
