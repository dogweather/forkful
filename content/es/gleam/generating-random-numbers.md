---
title:                "Generando números aleatorios"
html_title:           "Gleam: Generando números aleatorios"
simple_title:         "Generando números aleatorios"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

# ¡Generando Números Aleatorios con Gleam!

Si eres un programador, es muy probable que hayas escuchado hablar de la generación de números aleatorios. Pero, ¿qué es exactamente? Y más importante aún, ¿por qué los programadores lo hacen? En resumen, la generación de números aleatorios es un proceso en el que se producen números de forma aleatoria, sin seguir un patrón o secuencia predecible. Los programadores usan esto para varios propósitos, como juegos, simulaciones y criptografía.

## ¡Así se hace!

Para generar números aleatorios en Gleam, podemos utilizar la biblioteca `random` incorporada. Aquí hay un ejemplo de cómo podemos generar un número aleatorio entre 1 y 10:

```Gleam
import gleam/random
let mi_numero_aleatorio = random.int(1,10)
```

El resultado será un número aleatorio entre 1 y 10.

## Un Vuelco Profundo

La generación de números aleatorios puede parecer algo sencillo, pero tiene una interesante historia detrás. Originalmente, se utilizaban métodos como arrojar monedas o lanzar dados para producir números aleatorios. Con el avance de la tecnología, se desarrollaron algoritmos y computadoras para generar números aleatorios. Sin embargo, estos algoritmos se basan en datos conocidos, lo que significa que los resultados no son totalmente aleatorios. Para resolver este problema, se han desarrollado generadores de números aleatorios verdaderamente aleatorios, utilizando fuentes externas como la radioactividad o el ruido atmosférico.

Existen muchas alternativas para generar números aleatorios en Gleam, como la biblioteca `rand` y la función `random.float`. Además, los programadores pueden implementar sus propios algoritmos para generar números aleatorios en función de sus necesidades específicas.

## ¡Más Información!

Para obtener más información sobre la generación de números aleatorios en Gleam, puedes consultar la documentación oficial de IBM sobre la biblioteca `random` (https://gleam.run/docs/gleam/random.html). También puedes revisar la implementación del módulo `random` en el repositorio de Gleam en GitHub (https://github.com/gleam-lang/gleam_stdlib/blob/master/random/src/random.gleam).

¡Ahora estás preparado para generar números aleatorios en tus proyectos de Gleam! Recuerda que es importante entender cómo funcionan los generadores de números aleatorios y cuándo es necesario utilizarlos. ¡Diviértete explorando y experimentando con ellos!