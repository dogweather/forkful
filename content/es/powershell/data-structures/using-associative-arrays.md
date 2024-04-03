---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:12:29.326230-07:00
description: "Los arrays asociativos, tambi\xE9n conocidos como tablas hash o diccionarios\
  \ en PowerShell, te permiten almacenar datos en pares clave-valor, haciendo que\
  \ la\u2026"
lastmod: '2024-03-13T22:44:59.283201-06:00'
model: gpt-4-0125-preview
summary: "Los arrays asociativos, tambi\xE9n conocidos como tablas hash o diccionarios\
  \ en PowerShell, te permiten almacenar datos en pares clave-valor, haciendo que\
  \ la recuperaci\xF3n de datos sea directa y eficiente."
title: Uso de matrices asociativas
weight: 15
---

## Qué y Por Qué?

Los arrays asociativos, también conocidos como tablas hash o diccionarios en PowerShell, te permiten almacenar datos en pares clave-valor, haciendo que la recuperación de datos sea directa y eficiente. Los programadores los utilizan para almacenar datos relacionados juntos de una manera que es fácil de acceder por clave.

## Cómo hacerlo:

Crear y usar arrays asociativos en PowerShell es bastante sencillo. Aquí está cómo hacer la magia:

**Creando un array asociativo:**

```PowerShell
$myAssociativeArray = @{}
$myAssociativeArray["nombre"] = "Alex"
$myAssociativeArray["edad"] = 25
$myAssociativeArray["trabajo"] = "Ingeniero"
```

Este fragmento de código crea un array asociativo con tres pares clave-valor.

**Accediendo a los valores:**

Para obtener un valor, referencia su clave:

```PowerShell
Write-Output $myAssociativeArray["nombre"]
```

**Salida de muestra:**

```
Alex
```

**Añadiendo o modificando datos:**

Simplemente usa la clave para añadir un nuevo par o modificar uno existente:

```PowerShell
$myAssociativeArray["ubicación"] = "Nueva York" # Añade un nuevo par clave-valor
$myAssociativeArray["trabajo"] = "Ingeniero Senior" # Modifica un par existente
```

**Iterando sobre un array asociativo:**

Itera a través de las claves y valores así:

```PowerShell
foreach ($clave in $myAssociativeArray.Keys) {
  $valor = $myAssociativeArray[$clave]
  Write-Output "$clave : $valor"
}
```

**Salida de muestra:**

```
nombre : Alex
edad : 25
trabajo : Ingeniero Senior
ubicación : Nueva York
```

## Profundización

El concepto de arrays asociativos es común en muchos lenguajes de programación, normalmente llamado diccionario, mapa, o tabla hash dependiendo del lenguaje. En PowerShell, los arrays asociativos se implementan como tablas hash, que son bastante eficientes para buscar claves, almacenar datos y mantener una colección de claves únicas.

Históricamente, los arrays asociativos proveen un medio para manejar colecciones de objetos donde cada ítem puede ser rápidamente recuperado sin iterar a través de toda la colección, usando su clave. La eficiencia de recuperación y modificación de datos en arrays asociativos los hace una opción preferida para diversas tareas. Sin embargo, sí tienen limitaciones, como mantener el orden, para lo cual los diccionarios ordenados u objetos personalizados podrían ser una mejor alternativa.

A pesar de sus limitaciones, los arrays asociativos/tablas hash en PowerShell son increíblemente flexibles y una herramienta poderosa para scripting. Permiten el almacenamiento de datos dinámicos y son particularmente útiles en configuraciones, manipulación de datos y en cualquier lugar donde se necesite un formato de datos estructurado sin la sobrecarga de una definición de clase formal. Solo recuerda, mientras que los arrays asociativos son perfectos para la recuperación basada en clave, si tu tarea involucra estructuras de datos complejas o requiere mantener un orden específico, quizás quieras explorar otros tipos de datos u objetos personalizados dentro de PowerShell.
