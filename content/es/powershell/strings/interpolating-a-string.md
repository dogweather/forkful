---
date: 2024-01-20 17:51:22.012873-07:00
description: "Interpolar una cadena es insertar variables o expresiones dentro de\
  \ una cadena de texto. Los programadores lo hacen para construir mensajes din\xE1\
  micos sin\u2026"
lastmod: '2024-03-13T22:44:59.276283-06:00'
model: gpt-4-1106-preview
summary: Interpolar una cadena es insertar variables o expresiones dentro de una cadena
  de texto.
title: "Interpolaci\xF3n de cadenas de texto"
weight: 8
---

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
