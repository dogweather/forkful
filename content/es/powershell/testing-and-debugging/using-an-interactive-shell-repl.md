---
aliases:
- /es/powershell/using-an-interactive-shell-repl/
date: 2024-01-26 04:16:37.517542-07:00
description: "La shell interactiva, o Bucle Leer-Evaluar-Imprimir (REPL, por sus siglas\
  \ en ingl\xE9s), te permite escribir comandos de PowerShell y obtener\u2026"
lastmod: 2024-02-18 23:09:10.217231
model: gpt-4-0125-preview
summary: "La shell interactiva, o Bucle Leer-Evaluar-Imprimir (REPL, por sus siglas\
  \ en ingl\xE9s), te permite escribir comandos de PowerShell y obtener\u2026"
title: Usando una shell interactiva (REPL)
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
La shell interactiva, o Bucle Leer-Evaluar-Imprimir (REPL, por sus siglas en inglés), te permite escribir comandos de PowerShell y obtener retroalimentación inmediata. Los programadores la utilizan para probar rápidamente fragmentos de código, depurar o aprender nuevos comandos sin necesidad de escribir un script completo.

## Cómo hacerlo:
Inicia PowerShell y estarás en la REPL. Prueba el Cmdlet `Get-Date`:

```PowerShell
PS > Get-Date
```

Deberías ver la fecha y hora actuales:

```PowerShell
Miércoles, 31 de marzo de 2023 12:34:56 PM
```

Ahora, encadena comandos. Vamos a ordenar los procesos por uso de memoria:

```PowerShell
PS > Get-Process | Sort-Object WS -Descending | Select-Object -First 5
```

Esto muestra los 5 procesos principales por tamaño del conjunto de trabajo (uso de memoria).

## Profundizando
La REPL de PowerShell tiene sus raíces en la shell de Unix y otras shells de lenguajes dinámicos como la de Python. Es un entorno de ejecución de comandos interactivo de un solo usuario. A diferencia de un lenguaje compilado donde escribes aplicaciones enteras y luego compilas, un entorno REPL te permite escribir y ejecutar código línea por línea. PowerShell también admite la ejecución de scripts para tareas más grandes.

Las alternativas para Windows incluyen el Símbolo del sistema o otras REPL específicas de lenguajes como IPython. En el mundo de Unix/Linux, shells como bash o zsh cumplen una función similar.

La implementación de PowerShell utiliza una aplicación host para ejecutar la shell. Aunque PowerShell.exe en Windows es la más común, otros como el Entorno de Scripting Integrado (ISE) o el terminal integrado de Visual Studio Code también pueden servir como host.

## Ver También
- [Acerca de PowerShell](https://docs.microsoft.com/en-us/powershell/scripting/overview)
- [StackOverflow: PowerShell](https://stackoverflow.com/questions/tagged/powershell)
