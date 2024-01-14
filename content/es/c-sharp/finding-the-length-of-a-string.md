---
title:    "C#: Encontrando la longitud de una cadena"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## ¿Por qué?

En la programación, a menudo necesitamos saber la cantidad de caracteres en una cadena de texto. Ya sea para validar la entrada del usuario o para realizar cálculos, conocer la longitud de una cadena es una habilidad importante en el desarrollo de software.

## Cómo hacerlo

La forma más sencilla de encontrar la longitud de una cadena en C# es utilizando el método `Length` que está disponible en la clase `String`. Este método devuelve un entero que representa la cantidad de caracteres en la cadena dada.

```C#
    string texto = "¡Hola, mundo!";
    Console.WriteLine(texto.Length);  // output: 13
```

También podemos utilizar la propiedad `Length` en un bucle `for` para iterar sobre cada uno de los caracteres de la cadena.

```C#
    string texto = "abcde";
    
    for(int i = 0; i < texto.Length; i++) {
        Console.WriteLine(texto[i]);  // output: a, b, c, d, e
    }
```

Otra forma de obtener la longitud de una cadena es utilizando el método `GetLength()` en la clase `Array`. Este método nos permite pasar la dimensión de la cadena, en este caso la primera dimensión, y nos devuelve un entero con la longitud de la cadena.

```C#
    string[] nombres = {"Juan", "María", "Pedro"};
    Console.WriteLine(nombres.GetLength(0));  // output: 3
```

## Profundizando

Ahora que sabemos cómo encontrar la longitud de una cadena, es importante entender cómo funciona detrás de escena. En C#, una cadena de texto se representa como una secuencia de caracteres con un carácter de terminación especial `\0`. El método `Length` cuenta los caracteres en la cadena hasta que encuentra ese carácter de terminación, por lo que ese carácter no se incluye en el resultado final.

También es importante tener en cuenta que en C#, una cadena de texto es inmutable, lo que significa que no se pueden modificar los caracteres individuales de la cadena. Por lo tanto, cuando se cambia la longitud de una cadena, se crea una nueva cadena en lugar de modificar la original.

## Ver también

- [Documentación oficial de C# - Métodos de cadena](https://docs.microsoft.com/es-es/dotnet/api/system.string?view=net-5.0#methods)
- [Tutorial de programación en C# - Strings](https://www.tutorialspoint.com/csharp/csharp_strings.htm)
- [Foro de C# en español](https://foro.lawebdelprogramador.com/c/40/c-sharp/)