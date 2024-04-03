---
date: 2024-01-26 01:09:10.621730-07:00
description: "C\xF3mo hacerlo: Imagina que tienes un c\xF3digo que imprime un saludo\
  \ varias veces. Sin funciones, es un desorden. Con funciones, est\xE1 ordenado."
lastmod: '2024-03-13T22:44:59.083507-06:00'
model: gpt-4-1106-preview
summary: "Imagina que tienes un c\xF3digo que imprime un saludo varias veces."
title: "Organizando c\xF3digo en funciones"
weight: 18
---

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
