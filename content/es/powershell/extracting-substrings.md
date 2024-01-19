---
title:                "Extrayendo subcadenas"
html_title:           "C: Extrayendo subcadenas"
simple_title:         "Extrayendo subcadenas"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/powershell/extracting-substrings.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Extraer subcadenas es el acto de seleccionar y recuperar partes específicas de una cadena de texto. Los programadores lo hacen para manipular y analizar datos más efectivamente.

## Cómo se hace:

En PowerShell, el método `.substring` te permite extraer subcadenas. Aquí te dejo un ejemplo rápido:

```PowerShell
$s = "Esta es una prueba"
$subcadena = $s.Substring(0, 4)
echo $subcadena
```

En este caso, `$subcadena` devolvería `Esta`, ya que estamos comenzando en el índice 0 y especificando que queremos los próximos 4 caracteres.

## En Profundidad:

Históricamente, la extracción de subcadenas ha sido una técnica utilizada en programación desde los primeros días de la informática. En el contexto de PowerShell, la extracción de subcadenas fue implementada desde su primera versión y ha sido parte de su funcionalidad estándar desde entonces.

En cuanto a alternativas, puedes usar `-split` y `-replace` en PowerShell para lograr efectos similares en ciertos casos. Sin embargo, `.substring` es más preciso y sencillo para extraer una sección exacta de una cadena.

La implementación del método `.substring` en PowerShell se basa en el .NET Framework, lo que significa que también puede ser utilizado de la misma manera en C# y otros lenguajes .NET.

## Ver También:

Para más información, consulta las fuentes oficiales de Microsoft:

1. [Documentación de Microsoft .NET - Substring](https://docs.microsoft.com/es-es/dotnet/api/system.string.substring?view=net-5.0)
2. [Documentación de PowerShell - Acerca de las Cadenas](https://docs.microsoft.com/es-es/powershell/scripting/learn/deep-dives/everything-about-string-substitutions?view=powershell-7.1)
3. [Documentación de PowerShell - Split y Replace](https://docs.microsoft.com/es-es/powershell/module/microsoft.powershell.core/about/about_split?view=powershell-7.1)