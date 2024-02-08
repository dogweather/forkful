---
title:                "Organizando código en funciones"
date:                  2024-01-26T01:09:10.621730-07:00
model:                 gpt-4-1106-preview
simple_title:         "Organizando código en funciones"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
Organizar el código en funciones es como ordenar ladrillos de LEGO en cajas: facilita su búsqueda y utilización. Hacemos esto para evitar la repetición, para simplificar la comprensión y para que el mantenimiento sea menos problemático.

## Cómo hacerlo:
Imagina que tienes un código que imprime un saludo varias veces. Sin funciones, es un desorden. Con funciones, está ordenado.

```C#
// Sin funciones - repetitivo
Console.WriteLine("¡Hola, Amy!");
Console.WriteLine("¡Hola, Bob!");
Console.WriteLine("¡Hola, Charlie!");

// Con funciones - más limpio
void Saludar(string nombre) {
    Console.WriteLine($"¡Hola, {nombre}!");
}

Saludar("Amy");
Saludar("Bob");
Saludar("Charlie");
```

La salida es la misma, pero la segunda versión es mucho más ordenada.

## Estudio Profundo
Allá atrás, en los días del lenguaje ensamblador, saltabas a diferentes partes del código con GOTO—caótico y difícil de seguir. Las funciones son un avance importante, como cajones organizados en una caja de herramientas. ¿Alternativas? Claro. Tienes métodos, que son funciones en un contexto de clase. Luego están las lambdas y las funciones en línea para tareas rápidas y únicas.

Sobre la implementación: las funciones pequeñas y enfocadas son oro. Son más fáciles de probar y depurar. Las funciones grandes con muchas responsabilidades pueden convertirse en monstruosidades, ganando el dudoso título de "código espagueti". Mantente con un trabajo por función; te lo agradecerás más tarde.

## Ver También
Para más información sobre funciones y mejores prácticas, consulta:

- Código Limpio de Robert C. Martin: Principios para mantener tus funciones ordenadas.
- Refactorización de Martin Fowler: Maneras de mejorar el código existente.
- Guía de C# de Microsoft sobre Métodos: https://docs.microsoft.com/es-es/dotnet/csharp/programming-guide/classes-and-structs/methods
