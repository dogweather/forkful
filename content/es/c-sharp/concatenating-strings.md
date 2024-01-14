---
title:                "C#: Uniendo cadenas"
simple_title:         "Uniendo cadenas"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por qué
Al programar en C#, es común que necesitemos combinar diferentes cadenas de texto para crear un resultado final. Esto se conoce como concatenación de cadenas y es una técnica importante en la programación. En este artículo, vamos a explorar cómo se realiza la concatenación de cadenas en C# y por qué es una habilidad valiosa para cualquier programador.

## Cómo hacerlo
Para concatenar cadenas en C#, utilizamos el operador "+" o el método String.Concat(). Veamos un ejemplo de cada uno:

```
// Utilizando el operador "+"
string nombre = "Juan";
string apellido = "Pérez";
string nombreCompleto = nombre + " " + apellido;

Console.WriteLine(nombreCompleto); // Output: Juan Pérez

// Utilizando el método String.Concat()
string ciudad = "Madrid";
string país = "España";
string ubicación = String.Concat(ciudad, ", ", país);

Console.WriteLine(ubicación); // Output: Madrid, España
```

En ambos casos, podemos ver que hemos combinado diferentes cadenas para crear un nuevo valor. También podemos utilizar el método String.Format() para concatenar cadenas en un formato específico:

```
string mensaje = String.Format("Hola {0}, tu ciudad actual es {1}", nombre, ciudad);

Console.WriteLine(mensaje); // Output: Hola Juan, tu ciudad actual es Madrid
```

En este ejemplo, utilizamos los marcadores de posición {0} y {1} para indicar dónde deben ir los valores de las variables nombre y ciudad.

## Profundizando
Cuando trabajamos con la concatenación de cadenas en C#, es importante tener en cuenta que cada vez que concatenamos una cadena, se crea una nueva instancia de la misma. Esto puede no ser un problema en casos simples, pero puede causar problemas de rendimiento en casos más complejos. Por esta razón, se recomienda utilizar el método String.Concat() en lugar del operador "+" cuando se trate de concatenar más de dos cadenas.

Otra cosa a tener en cuenta es que el operador "+" también puede ser utilizado para convertir diferentes tipos de datos en cadenas. Por ejemplo:

```
int edad = 30;
string mensaje = "Tengo " + edad + " años";

Console.WriteLine(mensaje); // Output: Tengo 30 años
```

En este caso, el operador "+" convierte automáticamente el tipo de dato entero en una cadena para que pueda ser concatenado con las otras cadenas.

## Ver también
- [Documentación de Microsoft sobre concatenación de cadenas en C#](https://docs.microsoft.com/es-es/dotnet/csharp/programming-guide/strings/#concatenation)
- [Ejemplos de concatenación de cadenas en C#](https://www.programiz.com/csharp-programming/string-concatenation)
- [Método String.Concat() en la documentación de Microsoft](https://docs.microsoft.com/es-es/dotnet/api/system.string.concat)