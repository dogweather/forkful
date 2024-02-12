---
title:                "Interpolación de cadenas de texto"
aliases: - /es/powershell/interpolating-a-string.md
date:                  2024-01-20T17:51:22.012873-07:00
model:                 gpt-4-1106-preview
simple_title:         "Interpolación de cadenas de texto"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/powershell/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Qué es y Por Qué?

Interpolar una cadena es insertar variables o expresiones dentro de una cadena de texto. Los programadores lo hacen para construir mensajes dinámicos sin concatenar trozos de cadenas constantemente, simplificando así el código.

## Cómo Hacerlo:

Aquí te voy a mostrar cómo interpolar cadenas en tus scripts de PowerShell. Echale un vistazo:

```PowerShell
$nombre = "Miguel"
$edad = 30
$saludo = "Hola, mi nombre es $nombre y tengo $edad años."
Write-Output $saludo
```
Salida esperada:
```
Hola, mi nombre es Miguel y tengo 30 años.
```

También puedes utilizar expresiones dentro de la cadena:

```PowerShell
$precio = 15.99
$mensaje = "El total con impuesto es $($precio * 1.21)"
Write-Output $mensaje
```
Salida esperada:
```
El total con impuesto es 19.3469
```

## Profundizando

Históricamente, antes de que la interpolación de cadenas fuera común, los programadores concatenaban variables y cadenas, lo cual podía ser tedioso. En PowerShell, la interpolación se hace sencilla gracias a las comillas dobles, que permiten incluir directamente las variables dentro del texto.

Alternativas a la interpolación en PowerShell podrían incluir el uso del operador `-f`, que funciona de manera similar a `printf` en otros lenguajes:

```PowerShell
$nombre = "Miguel"
$edad = 30
$saludo = "Hola, mi nombre es {0} y tengo {1} años." -f $nombre, $edad
Write-Output $saludo
```

En cuanto a los detalles de implementación, la interpolación en PowerShell evalúa todo lo que esté dentro de `$()` como una expresión, lo que significa que puedes incluso realizar operaciones complejas y llamar funciones dentro de una cadena.

## Vea También

Para más información, puedes visitar la documentación oficial de PowerShell:
