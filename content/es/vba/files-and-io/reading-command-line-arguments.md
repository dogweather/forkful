---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:59:11.738958-07:00
description: "C\xF3mo hacerlo: A diferencia de entornos de programaci\xF3n m\xE1s\
  \ directos, VBA no tiene una funci\xF3n integrada para leer directamente los argumentos\
  \ de la l\xEDnea\u2026"
lastmod: '2024-03-13T22:44:58.910319-06:00'
model: gpt-4-0125-preview
summary: "A diferencia de entornos de programaci\xF3n m\xE1s directos, VBA no tiene\
  \ una funci\xF3n integrada para leer directamente los argumentos de la l\xEDnea\
  \ de comandos en un sentido convencional porque est\xE1 dise\xF1ado principalmente\
  \ para incrustarse dentro de aplicaciones de Microsoft Office."
title: "Leyendo argumentos de la l\xEDnea de comandos"
weight: 23
---

## Cómo hacerlo:
A diferencia de entornos de programación más directos, VBA no tiene una función integrada para leer directamente los argumentos de la línea de comandos en un sentido convencional porque está diseñado principalmente para incrustarse dentro de aplicaciones de Microsoft Office. Sin embargo, con un poco de creatividad, podemos utilizar el Host de Script de Windows (WSH) o llamar a APIs externas para lograr una funcionalidad similar. Aquí hay una solución práctica utilizando WSH:

1. **Crear un VBScript para Pasar Argumentos a VBA:**

   Primero, escribe un archivo VBScript (*yourScript.vbs*) que lance tu aplicación VBA (por ejemplo, una macro de Excel) y pase los argumentos de la línea de comandos:

```vb
Set objExcel = CreateObject("Excel.Application")
objExcel.Workbooks.Open "C:\TuMacroWorkbook.xlsm"
objExcel.Run "TuNombreDeMacro", WScript.Arguments.Item(0), WScript.Arguments.Item(1)
objExcel.Quit
```

2. **Acceder a los Argumentos en VBA:**

   En tu aplicación VBA (*TuMacroWorkbook.xlsm*), modifica o crea la macro (*TuNombreDeMacro*) para aceptar parámetros:

```vb
Sub TuNombreDeMacro(arg1 As String, arg2 As String)
    MsgBox "Argumento 1: " & arg1 & " Argumento 2: " & arg2
End Sub
```

3. **Ejecutar Tu Script:**

   Ejecuta el VBScript desde la línea de comandos, pasando los argumentos según sea necesario:

```shell
cscript yourScript.vbs "Hola" "Mundo"
```

   Esto debería resultar en la ejecución de tu macro VBA con los argumentos "Hola" y "Mundo", mostrándolos en un cuadro de mensaje.

## Análisis profundo:
En el contexto histórico, VBA fue concebido para extender las capacidades de las aplicaciones de Microsoft Office, no como un entorno de programación independiente. Como tal, la interacción directa con la línea de comandos está fuera de su alcance principal, lo que explica la falta de soporte integrado para leer argumentos de la línea de comandos.

El método descrito anteriormente, aunque efectivo, es más una solución alternativa que una solución nativa, aprovechando el scripting externo para cerrar la brecha. Este enfoque puede introducir complejidad y preocupaciones de seguridad potenciales, ya que requiere habilitar macros y potencialmente bajar la configuración de seguridad para ejecutar.

Para tareas que dependen en gran medida de los argumentos de la línea de comandos o que necesitan una integración más fluida con el sistema operativo de Windows, otros lenguajes de programación como PowerShell o Python podrían ofrecer soluciones más robustas y seguras. Estas alternativas proporcionan soporte directo para los argumentos de la línea de comandos y son más adecuadas para aplicaciones independientes o scripts que requieren una entrada externa para modificar su comportamiento dinámicamente.
