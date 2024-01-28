---
title:                "Manejo de errores"
date:                  2024-01-26T00:52:54.803645-07:00
model:                 gpt-4-1106-preview
simple_title:         "Manejo de errores"
programming_language: "C#"
category:             "C#"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/handling-errors.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Manejar errores en C# es acerca de gestionar lo inesperado, como tropezar con los cordones de tus zapatos. Los programas pueden tropezarse con datos erróneos o conexiones defectuosas. Manejamos errores para evitar que nuestro software se estrelle de cara, permitiéndole recuperarse con elegancia.

## Cómo hacerlo:

Empecemos con un bloque try-catch. Es como poner una red de seguridad bajo un funambulista. Si se resbala, no cae al vacío—queda atrapado.

```C#
using System;

class EjemploManejoErrores {
    static void Main() {
        try {
            int[] numeros = {1, 2, 3};
            Console.WriteLine(numeros[5]);  // ¡Ups, índice fuera de los límites!
        } catch (IndexOutOfRangeException e) {
            Console.WriteLine("Se capturó un error: " + e.Message);
        }
    }
}
```

Salida de muestra cuando las cosas van mal:
```
Se capturó un error: El índice estaba fuera de los límites de la matriz.
```

Ahora añadimos un bloque finally—es lo que sucede pase lo que pase, como pagar impuestos.

```C#
try {
    // Código potencialmente problemático aquí
} catch (AlgunaExcepcionEspecifica e) {
    // Manejar ese error específico aquí
} finally {
    // Este código se ejecuta sin importar lo que suceda arriba
    Console.WriteLine("Esto siempre se ejecuta.");
}
```

## Estudio Profundo

El manejo de errores ha estado en C# desde su nacimiento. Con el tiempo, ha evolucionado. En el pasado, los programadores se apoyaban en códigos de retorno o banderas globales para señalar problemas —torpe y propenso a errores.

C# utiliza excepciones, un enfoque más moderno. Una excepción se lanza cuando sucede lo inesperado, justo como lanzar una bandera en la jugada en el fútbol americano. El manejo estructurado de excepciones con bloques try, catch y finally hace la gestión de estos momentos más clara y limpia que el antiguo chequeo de errores.

¿Alternativas? Claro. Está el `UnhandledExceptionEventHandler` para excepciones que se escapan. O en código asíncrono, el manejo de errores se vuelve un poco al revés con objetos `Task` que llevan su propio equipaje de excepciones.

Los detalles de implementación, similares a la letra pequeña, importan. Las excepciones pueden ser costosas y disminuir el rendimiento si se lanzan a la ligera. Por lo tanto, las usamos para casos excepcionales, no para control de lógica cotidiano.

## Consulta También

- [Documentación oficial sobre Excepciones en C#](https://docs.microsoft.com/en-us/dotnet/csharp/fundamentals/exceptions/exception-handling)
- [Mejores prácticas en el manejo de excepciones en C#](https://docs.microsoft.com/en-us/dotnet/standard/exceptions/best-practices-for-exceptions)
