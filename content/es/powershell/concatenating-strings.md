---
title:                "Concatenando cadenas de texto"
html_title:           "Arduino: Concatenando cadenas de texto"
simple_title:         "Concatenando cadenas de texto"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/powershell/concatenating-strings.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Concatenar cadenas es el proceso de unir dos o más cadenas de texto en una. Los programadores lo hacen para manipular y combinarse información de texto de manera eficiente y precisa.

## ¿Cómo se hace?

Puedes concatenar cadenas en PowerShell utilizando el operador `+`. Veamos:

```PowerShell 
$cadena1 = "Hola "
$cadena2 = "mundo!"
$cadenaConcatenada = $cadena1 + $cadena2
echo $cadenaConcatenada
```

El resultado de este código será:

```PowerShell
Hola mundo!
```

También puedes usar el método `.Concat()`. Aquí está un ejemplo: 

```PowerShell
$cadena1 = "Hola "
$cadena2 = "mundo!"
$cadenaConcatenada = [string]::Concat($cadena1, $cadena2)
echo $cadenaConcatenada
```

Y el resultado será el mismo:

```PowerShell
Hola mundo!
```

## Inmersión Profunda

Concatenar cadenas de texto es una técnica que ha estado presente desde los primeros días de la programación. Aunque existen alternativas para unir cadenas en PowerShell, como `-join` y `-f`, utilizar `+` o `.Concat()` son los métodos más directos y sencillos.

Desde el punto de vista de la implementación, es importante tener en cuenta que concatenar cadenas puede ser costoso en términos de rendimiento, especialmente cuando se manejan cadenas de gran tamaño. PowerShell, como lenguaje basado en .NET, utiliza cadenas inmutables. Esto quiere decir que cada vez que concatenas cadenas, se crea una nueva, en lugar de modificar la existente.

## Consulta También 

1. Entrada de la documentación de Microsoft sobre cadenas de caracteres en PowerShell: [About Strings](https://docs.microsoft.com/es-es/powershell/module/microsoft.powershell.core/about/about_strings?view=powershell-7.1)
2. Blog técnico de Microsoft con detalles más profundos sobre la concatenación de cadenas: [ASCII Madness](https://blogs.technet.microsoft.com/heyscriptingguy/2013/08/26/ascii-madness-from-powershell/)
3. Documentación completa de PowerShell: [Documentación de PowerShell en Docs](https://docs.microsoft.com/es-es/powershell/scripting/overview?view=powershell-7.1)