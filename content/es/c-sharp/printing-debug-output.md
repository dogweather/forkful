---
title:                "Impresión de salida de depuración"
html_title:           "C#: Impresión de salida de depuración"
simple_title:         "Impresión de salida de depuración"
programming_language: "C#"
category:             "C#"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/printing-debug-output.md"
---

{{< edit_this_page >}}

¿Qué es imprimir salida de depuración y por qué lo hacen los programadores?
Imprimir salida de depuración es una técnica utilizada por los programadores para mostrar información durante el proceso de desarrollo de un programa, con el fin de identificar y corregir errores. Al agregar líneas de código para imprimir ciertos valores en la consola, los programadores pueden monitorear el comportamiento del programa y encontrar problemas que afectan su funcionamiento.
 
## ¿Cómo hacerlo?
Para imprimir en la consola en C#, podemos utilizar el método `Console.WriteLine()`, que imprimirá el valor pasado entre paréntesis en una nueva línea. Por ejemplo:
```C#
int numero = 5;
Console.WriteLine(numero);
```
Salida:
```
5
```
También podemos utilizar `Console.Write()` para imprimir en la misma línea. Por ejemplo:
```C#
string mensaje = "¡Hola, mundo!";
Console.Write(mensaje);
Console.Write(" Esto es una prueba.");
```
Salida:
```
¡Hola, mundo! Esto es una prueba.
```
## Profundizando
Antes de los entornos de desarrollo integrados (IDE) que tenemos hoy en día, imprimir salida de depuración era una técnica esencial para encontrar errores en el código. Los programadores tenían que utilizar herramientas específicas o imprimir la salida en un archivo para analizarla.

Hoy en día, existen otras opciones como el uso de puntos de interrupción o depuradores en tiempo real, que permiten a los programadores examinar el código paso a paso para encontrar errores. Sin embargo, imprimir salida de depuración sigue siendo una técnica común y útil.

## Mira también
- Documentación oficial de Microsoft sobre `Console.WriteLine()`: https://docs.microsoft.com/es-es/dotnet/api/system.console.writeline?view=netcore-3.1
- Tutorial de Codecademy sobre depuración de código: https://www.codecademy.com/es/courses/learn-c-sharp/lessons/depurar-csharp/exercises/depurar-csharp-intro