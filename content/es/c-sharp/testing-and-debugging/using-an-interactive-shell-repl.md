---
date: 2024-01-26 04:12:10.764374-07:00
description: "Un REPL, o Bucle Leer-Evaluar-Imprimir, te permite escribir c\xF3digo\
  \ C# y ejecutarlo de manera interactiva. Los programadores lo usan para experimentos\u2026"
lastmod: '2024-03-13T22:44:59.079785-06:00'
model: gpt-4-0125-preview
summary: "Un REPL, o Bucle Leer-Evaluar-Imprimir, te permite escribir c\xF3digo C#\
  \ y ejecutarlo de manera interactiva. Los programadores lo usan para experimentos\u2026"
title: Usando una shell interactiva (REPL)
---

{{< edit_this_page >}}

## Qué y Por Qué?
Un REPL, o Bucle Leer-Evaluar-Imprimir, te permite escribir código C# y ejecutarlo de manera interactiva. Los programadores lo usan para experimentos rápidos, depuración o aprendizaje de C#, sin la carga de configurar proyectos completos.

## Cómo hacerlo:
Inicia un REPL en tu entorno C# usando la ventana Interactiva de C# o ejecuta `dotnet-script` en tu terminal. Aquí tienes un ejemplo de su uso:

```csharp
> var saludo = "¡Hola, REPL!";
> Console.WriteLine(saludo);
¡Hola, REPL!
>
```

Obtienes retroalimentación instantánea. Sin pasos de compilación ni ejecución. Solo escribe código y observa.

## Profundización
REPL viajó desde Lisp hasta los lenguajes modernos, prosperando en aquellos dinámicos como Python. Con C#, Roslyn acercó el REPL a los desarrolladores. `csi` para Roslyn y `dotnet-script` para .NET Core, son opciones sólidas. Un corte más profundo: evalúan el código línea por línea, no todo junto, un modelo de ejecución diferente en comparación con las aplicaciones típicas de C#. Esto impacta en la persistencia del estado a través de las ejecuciones y el alcance de las variables.

La ventana Interactiva de C# de Visual Studio es un REPL potenciado por Roslyn. Cuenta con Intellisense, múltiples referencias y soporte de paquetes NuGet. Un gran avance respecto a los primeros experimentos de línea de comandos.

Para lenguajes alternativos, Python utiliza `IDLE`, JavaScript tiene el REPL de Node.js y F# viene con `F# Interactive`. Cada uno fomenta bucles de retroalimentación instantáneos, invaluables para probar pequeños fragmentos de código o comprender las características del lenguaje.

## Ver También
- [REPL `dotnet-script` de .NET Core](https://github.com/filipw/dotnet-script)
