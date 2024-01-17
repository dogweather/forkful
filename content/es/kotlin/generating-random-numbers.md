---
title:                "Generando números aleatorios"
html_title:           "Kotlin: Generando números aleatorios"
simple_title:         "Generando números aleatorios"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/generating-random-numbers.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Generar números aleatorios es una tarea común en la programación, que consiste en obtener valores numéricos sin un patrón o secuencia específica. Los programadores utilizan esta técnica para simular situaciones de la vida real, mejorar la seguridad de sus aplicaciones y crear variedad en sus programas.

## Cómo hacerlo:
Para generar un número aleatorio en Kotlin, podemos utilizar la función `random()` de la clase `kotlin.random.Random`. Esta función devuelve un valor aleatorio de tipo `Double` entre 0.0 y 1.0. A continuación, un ejemplo de cómo utilizar esta función y su correspondiente salida:

```Kotlin
val randomNum = kotlin.random.Random.random()
println(randomNum)

// Output:
0.7614217807816673 
```

## Profundizando:
La generación de números aleatorios ha sido una técnica ampliamente utilizada en la informática desde sus inicios. Una de las razones de su popularidad es que proporciona resultados impredecibles y no repetitivos, lo que es útil para simular situaciones reales, como el lanzamiento de un dado o la barajada de cartas.

Además de la función `random()` en Kotlin, también existen otras formas de generar números aleatorios. En algunos lenguajes de programación, como Java, se utiliza la clase `java.util.Random`, que ofrece una mayor variedad de opciones para generar diferentes tipos de datos numéricos. También hay bibliotecas de terceros disponibles en Kotlin, como `kotlinx.random`, que ofrecen algoritmos más avanzados para la generación de números aleatorios.

Para aquellos interesados en cómo funciona la generación de números aleatorios, es importante entender que sean generados realmente de manera aleatoria o si utilizan algún tipo de patrón o algoritmo para simular la aleatoriedad. En informática, no hay formas reales de generar números verdaderamente aleatorios, ya que dependen de algoritmos deterministas. Sin embargo, estos algoritmos están diseñados para generar valores que parezcan aleatorios e impredecibles.

## Ver también:
- [Documentación oficial de Kotlin sobre la función random()](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.random/random.html)
- [Clase Random en Java](https://docs.oracle.com/javase/8/docs/api/java/util/Random.html)
- [Biblioteca kotlinx.random](https://github.com/Kotlin/kotlinx.random)