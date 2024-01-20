---
title:                "Generando números aleatorios"
html_title:           "Arduino: Generando números aleatorios"
simple_title:         "Generando números aleatorios"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Generar números aleatorios implica la creación de una secuencia numérica que no sigue un patrón predecible. Los programadores lo hacen para garantizar la diversidad de resultados, manejar datos de prueba, o proveer imprevisibilidad en juegos y simulaciones.

## Cómo hacerlo:

La Fish Shell permite generar números aleatorios a través del comando `random`. Aquí tienes un ejemplo:

```fish
for i in (seq 5)
    echo (random 1 10)
end
```

Al ejecutar la secuencia anterior, se generarán cinco números aleatorios entre 1 y 10. Por ejemplo:

```fish
7
3
10
1
7
```

## Inmersión profunda:

Historicalmente, la generación de números aleatorios ha sido una necesidad esencial en las ciencias de la computación. En el contexto de la "Fish Shell", el comando `random` se introdujo en la versión 2.3.0, liberada en junio de 2016.

Existe un abanico de alternativas para generar números aleatorios en otros lenguajes y entornos. Python dispone de la función `random.randint()` y C++ ofrece la función `rand()` en la biblioteca `<cstdlib>`.

La implementación de la generación de números aleatorios en la "Fish Shell" emplea la función `rand()` de la biblioteca C del sistema, que será suficientemente precisa y aleatoria para la mayoría de los propósitos.

## Ver También:

Te invitamos a explorar más acerca de la generación de números aleatorios en los siguientes enlaces:

1. Manual de Fish Shell: https://fishshell.com/docs/current/commands.html#random
2. Generación de números aleatorios en Python: https://docs.python.org/3/library/random.html
3. Generación de números aleatorios en C++: http://www.cplusplus.com/reference/cstdlib/rand/