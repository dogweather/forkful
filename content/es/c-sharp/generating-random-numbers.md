---
title:                "Generando números aleatorios"
html_title:           "C#: Generando números aleatorios"
simple_title:         "Generando números aleatorios"
programming_language: "C#"
category:             "C#"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/generating-random-numbers.md"
---

{{< edit_this_page >}}

¡Hola lectores! En este artículo de programación, vamos a hablar sobre la generación de números aleatorios en C#, de manera informal y directa. No perderemos tiempo con palabras o frases innecesarias, ¡así que comenzaremos de inmediato!

## ¿Qué y por qué?

Generar números aleatorios es una herramienta muy útil en la programación, ya que nos permite obtener valores diferentes cada vez que se ejecuta un programa. Esto es especialmente útil en juegos y simulaciones, donde necesitamos resultados impredecibles para aumentar la diversión o la exactitud.

## ¡Cómo hacerlo!

En C#, generamos números aleatorios utilizando la clase ```Random```. Primero, debemos crear una instancia de esta clase y luego podemos utilizar sus métodos para obtener los números aleatorios deseados.

```c#
//Creamos una instancia de la clase Random
Random random = new Random();

//Obtenemos un número aleatorio entre 1 y 100
int numero = random.Next(1, 101);

//También podemos obtener un número aleatorio de tipo double
double numeroDecimal = random.NextDouble();

```

Y aquí tenemos un ejemplo de salida de estos códigos:

```
34
0.5623
```

## ¡Profundizando!

La generación de números aleatorios ha existido desde hace mucho tiempo en la informática, y ha evolucionado mucho desde entonces. En el pasado, se utilizaban tablas de números pre-generados para obtener resultados aleatorios. En la actualidad, existen varias bibliotecas y algoritmos altamente sofisticados para generar números aleatorios.

En C#, además de la clase ```Random```, también podemos utilizar la clase ```RNGCryptoServiceProvider``` para generar números aleatorios criptográficamente seguros.

## ¡Vea también!

Si quieres seguir aprendiendo sobre la generación de números aleatorios en C#, aquí te dejo algunos recursos adicionales:

- [Documentación oficial de Microsoft sobre la clase Random](https://docs.microsoft.com/en-us/dotnet/api/system.random?view=netframework-4.8)
- [Artículo sobre la generación de números aleatorios en C# de GeeksforGeeks](https://www.geeksforgeeks.org/ways-to-generate-random-numbers-in-c-sharp/)
- [Video tutorial sobre la generación de números aleatorios en C# de C# Corner](https://www.c-sharpcorner.com/article/random-number-generator-in-c-sharp/)

¡Eso es todo por ahora! Espero que este artículo te haya sido útil y puedas implementar la generación de números aleatorios en tus proyectos de C# de manera sencilla. ¡Hasta la próxima!