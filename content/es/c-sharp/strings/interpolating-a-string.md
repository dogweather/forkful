---
aliases:
- /es/c-sharp/interpolating-a-string/
date: 2024-01-20 17:50:42.503816-07:00
description: "Interpolar una cadena en C# significa incrustar expresiones dentro de\
  \ una cuerda literal. Lo hacemos para construir din\xE1micamente cadenas de texto,\u2026"
lastmod: 2024-02-18 23:09:09.967133
model: gpt-4-1106-preview
summary: "Interpolar una cadena en C# significa incrustar expresiones dentro de una\
  \ cuerda literal. Lo hacemos para construir din\xE1micamente cadenas de texto,\u2026"
title: "Interpolaci\xF3n de cadenas de texto"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Interpolar una cadena en C# significa incrustar expresiones dentro de una cuerda literal. Lo hacemos para construir dinámicamente cadenas de texto, combinando variables y literales con un código limpio y legible.

## Cómo hacerlo:
```C#
string nombre = "Juan";
int edad = 28;
string saludo = $"Hola, {nombre}! Tienes {edad} años.";

Console.WriteLine(saludo);
```
Salida:
```
Hola, Juan! Tienes 28 años.
```

## Análisis Profundo:
La interpolación de cadenas fue introducida en C# 6.0 como una característica que mejora la legibilidad y simplicidad al formatear cadenas. Antes de eso, utilizábamos `String.Format` o concatenación con el operador `+`. La interpolación se realiza en tiempo de ejecución y el compilador la transforma en un llamado a `String.Format`, pero con una sintaxis mucho más limpia. Por ejemplo:

```C#
string mensaje = String.Format("Hola, {0}! Tienes {1} años.", nombre, edad);
```
Esto hace la misma operación que el ejemplo de interpolación, pero es más verboso y propenso a errores.

La implementación de la interpolación de cadenas utiliza lo que se conoce como *interpolated strings*, que son reconocidos por el prefijo `$` antes de la comilla inicial.

El lenguaje C# también ofrece opciones avanzadas como la interpolación de cadenas con formato, donde puedes controlar la presentación de los datos interpolados. Por ejemplo:

```C#
double altura = 1.8;
string mensajeAltura = $"Tu altura es {altura:F2} metros.";
// F2 indica que queremos dos decimales
Console.WriteLine(mensajeAltura);
```

Salida:
```
Tu altura es 1.80 metros.
```

Además, C# 8.0 introdujo las *interpolated verbatim strings* que combinan la interpolación con la capacidad de tener cadenas literales que no procesan secuencias de escape.

## Ver También:
- Documentación de Microsoft sobre la interpolación de cadenas en C#: https://docs.microsoft.com/es-es/dotnet/csharp/language-reference/tokens/interpolated
- Guía de C# sobre `String.Format`: https://docs.microsoft.com/es-es/dotnet/standard/base-types/composite-formatting
- Información sobre *interpolated verbatim strings*: https://docs.microsoft.com/es-es/dotnet/csharp/language-reference/tokens/interpolated-verbatim
