---
title:                "C#: Uniendo cadenas de texto"
programming_language: "C#"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/concatenating-strings.md"
---

{{< edit_this_page >}}

## ¿Por qué deberías concatenar cadenas en C#?

Concatenar cadenas en C# es una habilidad esencial que todos los programadores deben dominar. Esta técnica te permite combinar y unir diferentes cadenas de texto para crear una cadena más grande y compleja. Esto es especialmente útil cuando se trabaja con datos dinámicos o cuando se necesita formatear la salida de un programa.

## Cómo hacerlo en C#

La concatenación de cadenas se realiza utilizando el operador `+` en C#. A continuación, te mostramos un ejemplo de código de cómo concatenar dos cadenas:
```C#
string nombre = "Juan";
string apellido = "García";
string nombreCompleto = nombre + " " + apellido;
Console.Write(nombreCompleto); // Output: Juan García
```

Como puedes ver, hemos utilizado el operador `+` para unir las cadenas `nombre` y `apellido` con un espacio en blanco entre ellas. También podrías utilizar la función `string.Concat()` para lograr el mismo resultado:
```C#
string nombre = "Juan";
string apellido = "García";
string nombreCompleto = string.Concat(nombre, " ", apellido);
Console.Write(nombreCompleto); // Output: Juan García
```

Incluso puedes concatenar más de dos cadenas a la vez:
```C#
string texto1 = "¡Hola ";
string texto2 = "amigos!";
string texto3 = " ¿Cómo están?";
string saludo = string.Concat(texto1, texto2, texto3);
Console.Write(saludo); // Output: ¡Hola amigos! ¿Cómo están?
```

## Un vistazo más profundo a la concatenación de cadenas

Ahora que ya sabes cómo concatenar cadenas en C#, es importante tener en cuenta algunas limitaciones. Por ejemplo, si estás concatenando un gran número de cadenas, es mejor utilizar la clase `StringBuilder` en lugar de usar repetidamente el operador `+`. Esto se debe a que la clase `StringBuilder` está optimizada para la construcción de cadenas largas y complejas.

Además, es importante tener en cuenta que la concatenación de cadenas es un proceso relativamente costoso en términos de rendimiento. Siempre que sea posible, es preferible utilizar la función `string.Format()` para formatear cadenas en lugar de concatenarlas repetidamente.

## Ver también

- [Documentación oficial de Microsoft sobre la concatenación de cadenas en C#](https://docs.microsoft.com/es-es/dotnet/csharp/programming-guide/strings/#concatenation)
- [Cómo usar la clase StringBuilder en C#](https://www.c-sharpcorner.com/article/using-stringbuilder-in-C-Sharp/)
- [Comparación de rendimiento entre la concatenación de cadenas y la función string.Format()](https://stackoverflow.com/questions/1042956/string-concatenation-vs-string-builder-which-is-faster)