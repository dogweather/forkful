---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:17:35.803196-07:00
description: "C\xF3mo hacerlo: En PowerShell, puedes usar los operadores `-match`,\
  \ `-replace` y `-split`, entre otros, para realizar acciones con expresiones regulares.\u2026"
lastmod: '2024-03-13T22:44:59.280318-06:00'
model: gpt-4-0125-preview
summary: En PowerShell, puedes usar los operadores `-match`, `-replace` y `-split`,
  entre otros, para realizar acciones con expresiones regulares.
title: Usando expresiones regulares
weight: 11
---

## Cómo hacerlo:
En PowerShell, puedes usar los operadores `-match`, `-replace` y `-split`, entre otros, para realizar acciones con expresiones regulares. Exploremos algunos ejemplos:

### Usar `-match` para verificar si una cadena coincide con un patrón
Este operador devuelve `$true` si el patrón se encuentra dentro de la cadena, y `$false` en caso contrario.

```powershell
"hello world" -match "\w+orld"
# Salida: True
```

### Extrayendo coincidencias
Puedes extraer el valor coincidente accediendo a la variable automática `$matches`.

```powershell
if ("I have 100 apples" -match "\d+") {
    "Número encontrado: " + $matches[0]
}
# Salida: Número encontrado: 100
```

### Usar `-replace` para sustituciones
El operador `-replace` reemplaza todas las ocurrencias de un patrón con una cadena de reemplazo especificada.

```powershell
"foo bar baz" -replace "ba[rz]", "qux"
# Salida: foo qux qux
```

### Dividir cadenas con `-split`
Divide una cadena en un arreglo de subcadenas basado en un patrón regex.

```powershell
"The quick-brown_fox jumps" -split "[-_ ]"
# Salida: The quick brown fox jumps
```

### Coincidencia de Patrones Avanzada
PowerShell también admite operaciones regex más complejas a través de la clase `[regex]`, dándote acceso a métodos como `Matches()`, `Replace()` y `Split()`.

```powershell
[regex]::Matches("June 24, August 9, Dec 12", "\b[A-Za-z]+\b").Value
# Salida: June August Dec

[regex]::Replace("100,000", "\B(?=(?:\d{3})+(?!\d))", ",")
# Salida: 100,000

[regex]::Split("one,two;three four", ",|;| ")
# Salida: one two three four
```

Estos ejemplos muestran el poder y la versatilidad de las expresiones regulares en PowerShell para la manipulación de datos y la coincidencia de patrones. Al utilizar regex, los programadores pueden realizar un procesamiento de texto complejo de manera eficiente.
