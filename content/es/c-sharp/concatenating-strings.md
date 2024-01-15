---
title:                "Concatenando cadenas"
html_title:           "C#: Concatenando cadenas"
simple_title:         "Concatenando cadenas"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/concatenating-strings.md"
---

{{< edit_this_page >}}

## ¿Por qué?

Hay muchas situaciones en las que puede ser necesario combinar o unir varias cadenas de texto en una sola. Puede ser para crear un mensaje personalizado, generar un identificador único o incluso para formatear una salida de datos. En este artículo, aprenderemos cómo hacerlo en C# de una manera sencilla y eficiente.

## Cómo hacerlo

La concatenación de cadenas en C# se realiza utilizando el operador `+` o el método `Concat()`. Veamos un ejemplo usando el operador `+`:

```C#
string nombre = "Juan";
string apellido = "Pérez";
string nombreCompleto = nombre + " " + apellido;
Console.WriteLine(nombreCompleto);
```

La salida de este código será "Juan Pérez".

También podemos usar el método `Concat()` para unir varias cadenas:

```C#
string mensaje1 = "¡Hola ";
string mensaje2 = "amigo!";
string mensajeCompleto = String.Concat(mensaje1, mensaje2);
Console.WriteLine(mensajeCompleto);
```

La salida de este código será "¡Hola amigo!".

Otra forma de unir cadenas es utilizando el método `Join()` que nos permite unir una colección de cadenas especificando un separador. Veamos un ejemplo:

```C#
string[] hobbies = { "fútbol", "lectura", "cocina" };
string hobbiesUnidos = String.Join(", ", hobbies);
Console.WriteLine(hobbiesUnidos);
```

La salida de este código será "fútbol, lectura, cocina".

## Profundizando

Es importante tener en cuenta que cada vez que unimos cadenas, se crea una nueva cadena en memoria. Esto puede ser ineficiente cuando se trabaja con grandes cantidades de datos. Para evitar esto, C# nos ofrece la clase `StringBuilder`, que nos permite manipular y unir cadenas de manera más eficiente. Veamos un ejemplo:

```C#
StringBuilder sb = new StringBuilder("¡Hola ");
sb.Append("amigo");
sb.Append("!");
Console.WriteLine(sb.ToString());
```

La salida de este código será "¡Hola amigo!".

También podemos utilizar interpolación de cadenas en C# 6 o superior para unir cadenas de manera más legible y eficiente. Veamos un ejemplo:

```C#
string nombre = "Juan";
string mensaje = $"¡Hola {nombre}, bienvenido!";
Console.WriteLine(mensaje);
```

La salida de este código será "¡Hola Juan, bienvenido!".

## Ver también

- Documentación oficial de Microsoft sobre la concatenación de cadenas en C#: https://docs.microsoft.com/es-es/dotnet/csharp/how-to/concatenate-multiple-strings
- Tutoriales y ejercicios prácticos sobre la concatenación de cadenas en C#: https://www.tutorialesprogramacionya.com/csharpya/detalleconcepto.php?punto=39&codigo=39&inicio=30