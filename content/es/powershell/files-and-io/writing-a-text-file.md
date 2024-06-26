---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:28:52.554308-07:00
description: "C\xF3mo hacerlo: PowerShell proporciona cmdlets sencillos para manejar\
  \ archivos. El cmdlet `Out-File` y los operadores de redirecci\xF3n se usan principalmente\u2026"
lastmod: '2024-03-13T22:44:59.310612-06:00'
model: gpt-4-0125-preview
summary: PowerShell proporciona cmdlets sencillos para manejar archivos.
title: Escribiendo un archivo de texto
weight: 24
---

## Cómo hacerlo:
PowerShell proporciona cmdlets sencillos para manejar archivos. El cmdlet `Out-File` y los operadores de redirección se usan principalmente para este propósito. Aquí hay ejemplos que ilustran cómo escribir texto en archivos en diferentes escenarios:

**Creación básica de un archivo de texto:**

Para crear un archivo de texto y escribir una cadena simple en él, puedes usar:

```powershell
"¡Hola, Mundo!" | Out-File -FilePath .\ejemplo.txt
```

O equivalentemente con el operador de redirección:

```powershell
"¡Hola, Mundo!" > .\ejemplo.txt
```

**Agregando texto a un archivo existente:**

Si quieres agregar texto al final de un archivo existente sin sobrescribirlo:

```powershell
"Otra línea." | Out-File -FilePath .\ejemplo.txt -Append
```

O utilizando el operador de redirección de agregado:

```powershell
"Otra línea." >> .\ejemplo.txt
```

**Escribiendo múltiples líneas:**

Para escribir múltiples líneas, puedes usar un arreglo de cadenas:

```powershell
$lineas = "Línea 1", "Línea 2", "Línea 3"
$lineas | Out-File -FilePath .\multilineas.txt
```

**Especificando la codificación:**

Para especificar una codificación de texto particular, usa el parámetro `-Encoding`:

```powershell
"Texto con codificación UTF8" | Out-File -FilePath .\ejemploutf.txt -Encoding UTF8
```

**Usando bibliotecas de terceros:**

Aunque los cmdlets integrados de PowerShell son suficientes para operaciones básicas de archivo, las tareas más complejas podrían beneficiarse de módulos de terceros como `PowershellGet` o herramientas como `SED` y `AWK` portadas para Windows. Sin embargo, para la escritura pura de un archivo de texto, estos podrían ser excesivos y generalmente no son necesarios:

```powershell
# Asumiendo un escenario más complejo que justifique el uso de una biblioteca externa
# Install-Module -Name AlgunaLibreriaCompleja
# Import-Module -Name AlgunaLibreriaCompleja
# Operaciones más complejas aquí
```

_Nota: Siempre considera si la complejidad de agregar una dependencia de terceros se justifica para tus necesidades._

**Salida de muestra:**

Después de ejecutar el comando de creación básica de archivos, al revisar el contenido de `ejemplo.txt` se muestra:

```plaintext
¡Hola, Mundo!
```

Para agregar texto y luego revisar `ejemplo.txt`:

```plaintext
¡Hola, Mundo!
Otra línea.
```
