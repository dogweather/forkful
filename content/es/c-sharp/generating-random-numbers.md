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

## ¿Por qué generar números aleatorios en C#?

Generar números aleatorios puede ser una tarea útil en la programación, ya sea para desarrollar juegos, aplicaciones de simulación o para probar diversas funcionalidades de un sistema. A través de este artículo, aprenderás cómo generar números aleatorios en C# y entenderás un poco más sobre cómo funcionan.

## Cómo hacerlo en C#

Para generar números aleatorios en C#, utilizaremos la clase `Random` incluida en el espacio de nombres `System`. Esta clase nos permite generar números enteros, números de punto flotante y cadenas de caracteres de forma aleatoria. Veamos algunos ejemplos:

```C#
// Generar un número entero aleatorio entre 1 y 10
Random random = new Random();
int numeroEntero = random.Next(1, 11); 
```
El método `Next()` toma dos parámetros, el primero es el número mínimo que queremos generar y el segundo es el número máximo, pero este último no está incluido en la lista de posibles resultados.

```C#
// Generar un número de punto flotante aleatorio entre 0 y 1
double numeroDecimal = random.NextDouble(); 
```

```C#
// Generar una cadena de 10 caracteres aleatorios
string cadenaAleatoria = new string(Enumerable.Repeat("0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ", 10)
  .Select(s => s[random.Next(s.Length)]).ToArray()); 
```

## Inmersión profunda en la generación de números aleatorios

La clase `Random` utiliza un valor semilla para generar números aleatorios. Si no le proporcionamos un valor, utilizará como semilla un número basado en la hora en la que se ejecuta el programa. Esto significa que si ejecutas el código varias veces en el mismo segundo, obtendrá el mismo resultado. Para evitar esto, podemos proporcionar nuestro propio valor semilla.

Otra cosa a tener en cuenta es que la clase `Random` no es realmente aleatoria, sino que utiliza un algoritmo para generar los números. Si necesitamos una verdadera aleatoriedad, podemos utilizar la clase `RNGCryptoServiceProvider` incluida en el espacio de nombres `System.Security.Cryptography`, que utiliza fuentes de entropía externas para generar los números.

## Ver también

- [Documentación oficial de la clase Random - Microsoft](https://docs.microsoft.com/en-us/dotnet/api/system.random?view=netframework-4.8)
- [Documentación oficial de la clase RNGCryptoServiceProvider - Microsoft](https://docs.microsoft.com/en-us/dotnet/api/system.security.cryptography.rngcryptoserviceprovider?view=netframework-4.8)