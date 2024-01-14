---
title:    "C#: Uniendo cadenas de texto"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por qué

La concatenación de strings es una técnica muy útil en la programación, ya que nos permite combinar diferentes strings en uno solo. Esto puede ser especialmente útil cuando necesitamos mostrar información en una sola línea, como en un programa de consola, o cuando queremos crear un string más complejo a partir de diferentes elementos.

## Cómo hacerlo

La concatenación de strings en C# es muy sencilla. Podemos utilizar el operador `+` para unir dos strings, o utilizar el método `Concat` de la clase `String` si queremos concatenar más de dos strings. Veamos un ejemplo:

```C#
string str1 = "¡Hola";
string str2 = "mundo!";
string str3 = "Soy un" + " ejemplo de concatenación."; // Utilizando el operador '+'
string str4 = String.Concat(str1, " ", str2, ". ", str3); // Utilizando el método 'Concat'

Console.WriteLine(str4);

// Salida:
// ¡Hola mundo! Soy un ejemplo de concatenación.
```

Como podemos ver en este ejemplo, podemos unir strings utilizando el operador `+` o utilizando el método `Concat`. En ambos casos, el resultado es el mismo: un solo string que contiene todas las cadenas concatenadas.

También podemos utilizar el método `Join` de la clase `String` si queremos unir un array de strings en uno solo. El primer argumento de este método es un string que se utilizará como separador entre los elementos del array. Veamos un ejemplo:

```C#
string[] palabras = {"estoy", "aprendiendo", "a", "programar"};
string frase = String.Join(" ", palabras);

Console.WriteLine(frase);

// Salida:
// estoy aprendiendo a programar
```

En este caso, el primer argumento del método `Join` es el espacio entre comillas `" "`, por lo que cada elemento del array será separado por un espacio en el string resultante.

## Un poco más profundo

Cuando concatenamos strings en C#, el compilador automáticamente convierte los valores al tipo `string`, por lo que podemos unir cualquier tipo de dato con un string, como por ejemplo, un número. Veamos un ejemplo:

```C#
int edad = 25;
string mensaje = "¡Tengo " + edad + " años!";

Console.WriteLine(mensaje);

// Salida:
// ¡Tengo 25 años!
```

También podemos utilizar el método `ToString()` para convertir otros tipos de datos a string antes de concatenarlos. Por ejemplo:

```C#
double pi = 3.141592;
string mensaje = "El valor de pi es: " + pi.ToString("#.##");

Console.WriteLine(mensaje);

// Salida:
// El valor de pi es: 3.14
```

Otra cosa importante a tener en cuenta es que la concatenación de strings crea un nuevo objeto en cada operación, por lo que si necesitamos unir muchas cadenas de forma eficiente, es mejor utilizar un `StringBuilder`, que es una clase optimizada para este propósito.

## Ver también

- Documentación oficial de Microsoft: [Concatenación de strings en C#](https://docs.microsoft.com/es-es/dotnet/csharp/programming-guide/strings/concatenation-of-strings)
- Tutorial de concatenación de strings en C#: [Concatenación de strings en C# (en español)](https://www.leoykuantu.com/2016/03/insertar-valores-de-tipos-de-datos.html)