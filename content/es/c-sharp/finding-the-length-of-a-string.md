---
title:                "C#: Encontrando la longitud de una cadena"
programming_language: "C#"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Por qué

¿Alguna vez has querido saber cuántos caracteres hay en una palabra o frase? En este artículo, aprenderás cómo encontrar la longitud de una cadena en C#. Esto es útil para muchas tareas de programación, como validar la entrada de un usuario o calcular el tamaño de una cadena para su almacenamiento.

## Cómo hacerlo

Para encontrar la longitud de una cadena en C#, podemos utilizar la función `Length` de la clase `String`. Esta función devuelve el número de caracteres en una cadena como un entero. Aquí hay un ejemplo de cómo usarlo:

```C#
string miCadena = "¡Hola Mundo!";
int longitud = miCadena.Length;
Console.WriteLine("La longitud de la cadena es: " + longitud); // salida: La longitud de la cadena es: 11
```

En este ejemplo, hemos creado una cadena `miCadena` con la frase "¡Hola Mundo!" y luego usamos la función `Length` para obtener su longitud. Después, imprimimos el valor de la longitud en la consola.

Pero, ¿qué pasa si queremos encontrar la longitud de una cadena que el usuario ingresa en tiempo de ejecución? En ese caso, podemos utilizar la función `ReadLine` de la clase `Console` para obtener la entrada del usuario y luego aplicar la función `Length` a esa entrada. Aquí hay un ejemplo:

```C#
Console.WriteLine("Ingresa una palabra: ");
string palabra = Console.ReadLine();
int longitud = palabra.Length;
Console.WriteLine("La longitud de la palabra es: " + longitud);
```

Este código primero le pide al usuario que ingrese una palabra, luego almacena la entrada en la variable `palabra`. Después, usamos la función `Length` para obtener la longitud de esa palabra y la imprimimos en la consola.

## Profundizando

Ahora que sabes cómo encontrar la longitud de una cadena en C#, aquí hay algunas cosas importantes a tener en cuenta:

- La función `Length` cuenta todos los caracteres de una cadena, incluyendo espacios en blanco y signos de puntuación.
- Si una cadena está vacía, es decir, no tiene ningún carácter, su longitud será 0.
- La función `Length` también funciona en cadenas con caracteres especiales, como letras acentuadas o emoji.

Ahora tienes las herramientas para encontrar fácilmente la longitud de cualquier cadena en C#. ¡Empieza a explorar!

## Ver también

- [C# Strings](https://docs.microsoft.com/es-es/dotnet/csharp/tour-of-csharp/strings)
- [La función Length de la clase String en C#](https://www.lawebdelprogramador.com/codigo/CSharp/3682-La-funcion-Length-de-la-clase-String-en-C.html)
- [Cómo encontrar la longitud de una cadena en C#](https://parzibyte.me/blog/2021/08/12/cadenas-csharp-length/)