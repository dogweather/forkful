---
title:                "Extracción de subcadenas"
date:                  2024-01-20T17:45:19.003405-07:00
model:                 gpt-4-1106-preview
simple_title:         "Extracción de subcadenas"

category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/extracting-substrings.md"
---

{{< edit_this_page >}}

## ¿Qué & Por Qué?
Extraer subcadenas implica tomar trozos de texto desde una cadena mayor. Los programadores lo hacen para analizar datos, validar entradas o simplemente para trabajar con partes específicas de texto de forma más cómoda.

## Cómo hacerlo:
Vamos a ver cómo extraer subcadenas en C# usando el método `Substring`. Aquí hay un ejemplo:

```C#
string fraseCompleta = "Hola, programadores!";
string saludo = fraseCompleta.Substring(0, 4); // Extrae "Hola"
string audiencia = fraseCompleta.Substring(7); // Extrae "programadores!"

Console.WriteLine(saludo);    // Output: Hola
Console.WriteLine(audiencia); // Output: programadores!
```

El primer número indica el inicio, y el segundo la longitud de la subcadena. Si omites el segundo, obtienes todo hasta el final.

## Profundizando:
Historialmente, extraer subcadenas ha sido esencial en la manipulación de textos. En versiones antiguas de C#, íbamos carácter por carácter. Ahora, con métodos como `Substring`, es pan comido.

Alternativamente podrías usar `Span<T>` o `Memory<T>` en situaciones donde el rendimiento es crítico – estos evitan crear copias de las subcadenas y son más eficientes.

En cuanto a los detalles de implementación, `Substring` puede lanzar `ArgumentOutOfRangeException` si los índices están mal. Por eso, siempre verifica los límites antes de extraer.

## Vea también:
- Documentación de Microsoft sobre [`String.Substring`](https://docs.microsoft.com/en-us/dotnet/api/system.string.substring?view=net-6.0)
- [`Span<T>` y `Memory<T>`](https://docs.microsoft.com/en-us/dotnet/standard/memory-and-spans/memory-t-usage-guidelines)
- Un tutorial sobre manipulación de cadenas en C#: [Manipulación de Cadenas en C#](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/strings/)
