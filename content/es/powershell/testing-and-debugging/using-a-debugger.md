---
date: 2024-01-26 04:08:41.031083-07:00
description: "Usar un depurador significa establecer puntos de interrupci\xF3n, avanzar\
  \ paso a paso por tu c\xF3digo, observar variables e inspeccionar el estado de tu\u2026"
lastmod: '2024-03-13T22:44:59.296751-06:00'
model: gpt-4-0125-preview
summary: "Usar un depurador significa establecer puntos de interrupci\xF3n, avanzar\
  \ paso a paso por tu c\xF3digo, observar variables e inspeccionar el estado de tu\u2026"
title: Usando un depurador
---

## Cómo hacerlo:
En PowerShell, puedes depurar scripts utilizando el Entorno de Scripting Integrado de PowerShell (ISE) incorporado o Visual Studio Code (VS Code) con la extensión de PowerShell. Aquí te explicamos cómo usar puntos de interrupción en ambos:

### PowerShell ISE:
```PowerShell
# Establece un punto de interrupción en una línea específica
Set-PSBreakpoint -Script .\MyScript.ps1 -Line 5

# Ejecuta tu script normalmente
.\MyScript.ps1

# Cuando el script llegue al punto de interrupción, puedes inspeccionar variables
$myVariable

# Continúa la ejecución
Continue
```

### Visual Studio Code:
```PowerShell
# Abre tu script de PowerShell en VS Code.
# Haz clic a la izquierda del número de línea para establecer un punto de interrupción.
# Inicia la depuración presionando F5 o haciendo clic en 'Iniciar Depuración'.

# VS Code detendrá la ejecución en tu punto de interrupción.
# Usa el panel de depuración para observar variables, inspeccionar la pila de llamadas y controlar el flujo.
```

La depuración en ambos entornos te permite entrar en (F11), pasar sobre (F10) y salir de (Shift+F11) durante la depuración.

## Profundización
Históricamente, la depuración en PowerShell era un poco torpe; requería muchas líneas de `Write-Host` para mostrar el estado de las variables o el clásico método de prueba y error. Con la llegada de PowerShell ISE, y más recientemente, VS Code con sus ricas características de depuración, la depuración de PowerShell se volvió casi tan intuitiva como en lenguajes de programación completos.

Alternativas a las herramientas de depuración nativas de PowerShell incluyen herramientas de terceros como PowerGUI o el uso de IDEs robustos como Visual Studio con un complemento de PowerShell.

Cuando implementes un depurador, considera el ámbito del script, especialmente cuando trabajes con scripts dot-sourced o módulos. Los puntos de interrupción pueden basarse en condiciones, cambios de variables o ser específicos de una línea, lo que permite un control preciso durante una sesión de depuración.

Además, con la transición a PowerShell Core (PowerShell multiplataforma), la depuración ha pasado en gran medida a manos de VS Code, que ofrece una experiencia consistente en diferentes plataformas.

## Ver También
Para más información sobre depuración en PowerShell:
- [about_Debuggers](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_Debuggers)
