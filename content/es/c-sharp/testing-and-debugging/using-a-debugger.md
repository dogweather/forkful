---
date: 2024-01-26 03:48:00.764822-07:00
description: "Imagina que tienes un programa peque\xF1o que no est\xE1 funcionando\
  \ correctamente: ```C# static void Main() { int resultado = Suma(1, 2);\u2026"
lastmod: '2024-03-13T22:44:59.082584-06:00'
model: gpt-4-0125-preview
summary: "Imagina que tienes un programa peque\xF1o que no est\xE1 funcionando correctamente:\
  \ ```C# static void Main() { int resultado = Suma(1, 2);\u2026"
title: Usando un depurador
weight: 35
---

## Cómo:
Imagina que tienes un programa pequeño que no está funcionando correctamente:

```C#
static void Main()
{
    int resultado = Suma(1, 2);
    Console.WriteLine(resultado);
}

static int Suma(int a, int b)
{
    return a + a; // Uy, debería ser a + b
}
```

Usando el depurador de Visual Studio, establece un punto de interrupción haciendo clic en el margen izquierdo junto a `return a + a;`. Cuando ejecutes el programa (con F5), la ejecución se pausará allí. Pasa el cursor sobre las variables para inspeccionar sus valores o utiliza la Ventana Inmediata para evaluar expresiones. Verás que `a` es 1 y `b` es 2, pero `a + a` no es la suma esperada. Cámbialo a `a + b`, continúa ejecutando (F5), y voilà, la consola muestra 3.

## Profundización
La historia de la depuración se remonta a la década de 1940 cuando un verdadero bug (una polilla) fue encontrado en una computadora temprana. Los depuradores de hoy, como el de Visual Studio, proporcionan un conjunto de características poderosas, incluidos puntos de interrupción, ejecución paso a paso, ventanas de inspección y más.

Alternativas al depurador de Visual Studio incluyen opciones de código abierto como GDB para lenguajes estilo C o pdb para Python, e IDEs multiplataforma como JetBrains Rider o VS Code, que ofrecen herramientas de depuración para C# y otros lenguajes.

Cuando te sumerges en la implementación de un depurador, estás mirando un programa que se adjunta al proceso de tu aplicación. Interpreta código máquina, maneja el estado de la memoria y controla el flujo de ejecución. Esto es material pesado que es crucial para una depuración efectiva, razón por la cual el modo de depuración a menudo funciona más lento que el modo de lanzamiento donde estos ganchos no existen.

## Ver También
- [Documentación del Depurador de Visual Studio](https://docs.microsoft.com/es-es/visualstudio/debugger/)
- [Estrategias de Depuración](https://www.codeproject.com/Articles/79508/Effective-Exception-Handling-in-Visual-C)
