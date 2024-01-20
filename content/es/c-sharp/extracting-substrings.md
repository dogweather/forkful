---
title:                "Extrayendo subcadenas"
html_title:           "C: Extrayendo subcadenas"
simple_title:         "Extrayendo subcadenas"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/extracting-substrings.md"
---

{{< edit_this_page >}}

## ¿Qué & Por Qué?
La extracción de subcadenas es el proceso de obtener una porción de una cadena existente. Los programadores lo hacen para manipular y operar en segmentos específicos de cadenas en lugar de la cadena completa.

## ¿Cómo hacerlo?
Aquí se muestra cómo extraer subcadenas en C#. Usaremos el método `Substring(int startIndex, int length)`.

```C#
string s = "El aprendizaje nunca se termina";
string substr = s.Substring(3, 11);
Console.WriteLine(substr);
```
Salida:
```
aprendizaje
```

## Inmersión Profunda
La funcionalidad de extracción de subcadenas ha estado presente en la mayoría de los lenguajes de programación desde los primeros días, y C# no es la excepción. Una alternativa al enfoque 'Substring' es usar `Split` para dividir la cadena en un arreglo y luego operar en elementos individuales. Sin embargo, este método puede tener un rendimiento más bajo especialmente en cadenas muy largas. En términos de detalles de implementación, el método `Substring` en C# realmente crea una nueva cadena y no modifica la cadena original, por lo tanto, es seguro usarlo sin preocuparse por los efectos secundarios.

`Split` ejemplo:
```C#
string s = "Hola:Mundo:!";
string[] palabras = s.Split(':');
Console.WriteLine(palabras[1]); 
```
Salida: 
```
Mundo
```

## Ver También
Para más detalles y usos de extraer subcadenas en C#, visita los siguientes enlaces:

1. [Tutorial de Microsoft - String.Substring Method in C#](https://docs.microsoft.com/en-us/dotnet/api/system.string.substring?view=net-5.0)
2. [StackOverflow - C# substring](https://stackoverflow.com/questions/7845675/how-to-use-the-substring-method-in-c-sharp)
3. [Información detallada sobre la cadena de C# en TutorialsPoint](https://www.tutorialspoint.com/csharp/csharp_strings.htm)