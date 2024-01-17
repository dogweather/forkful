---
title:                "Generando números aleatorios"
html_title:           "PowerShell: Generando números aleatorios"
simple_title:         "Generando números aleatorios"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/powershell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## ¡Qué y por qué?
Generar números aleatorios es un proceso en el que se crean números sin ningún patrón o predicción. Los programadores utilizan esto para simular situaciones aleatorias en sus aplicaciones, juegos o para crear contraseñas seguras.

## Cómo hacerlo:
### Ejemplo 1: Generar un solo número aleatorio
```PowerShell
Get-Random
```
Salida de muestra: 85

### Ejemplo 2: Generar una lista de números aleatorios
```PowerShell
Get-Random -Count 5 -Minimum 1 -Maximum 10
```
Salida de muestra: 4, 9, 2, 6, 8

### Ejemplo 3: Generar un número aleatorio entre dos valores
```PowerShell
Get-Random -Minimum 10 -Maximum 20
```
Salida de muestra: 15

## Profundizando:
### Contexto histórico:
La generación de números aleatorios ha sido una herramienta importante en la informática desde sus inicios. Anteriormente, se utilizaban técnicas como lanzar dados o utilizar tablas con números predefinidos. Hoy en día, la mayoría de los sistemas operativos tienen funciones incorporadas para generar números aleatorios y existen algoritmos más sofisticados para lograr una aleatoriedad más precisa.

### Alternativas:
Mientras que las funciones de generación de números aleatorios incorporadas en los sistemas operativos pueden ser suficientes para la mayoría de los casos, existen bibliotecas externas que ofrecen opciones más avanzadas para generar números pseudoaleatorios o números basados en fuentes de entropía como el movimiento del mouse.

### Detalles de implementación:
Internamente, la función Get-Random utiliza la clase System.Random de .NET. Esto significa que los números generados no son verdaderamente aleatorios, sino que siguen un algoritmo matemático predefinido. Esto puede ser un problema en aplicaciones que requieren una alta calidad de aleatoriedad, como en los juegos de azar.

## Ver también:
- [Documentación oficial de Microsoft para la función Get-Random] (https://docs.microsoft.com/powershell/module/microsoft.powershell.utility/get-random?view=powershell-7.1)
- [Artículo de Wikipedia sobre generación de números aleatorios] (https://es.wikipedia.org/wiki/Generaci%C3%B3n_de_n%C3%BAmeros_pseudoaleatorios)