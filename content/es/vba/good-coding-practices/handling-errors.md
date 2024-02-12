---
title:                "Manejo de errores"
aliases:
- /es/vba/handling-errors.md
date:                  2024-02-01T21:55:02.544800-07:00
model:                 gpt-4-0125-preview
simple_title:         "Manejo de errores"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/vba/handling-errors.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## ¿Qué y por qué?

El manejo de errores en Visual Basic para Aplicaciones (VBA) se refiere al proceso de anticipar, detectar y resolver errores de programación, de aplicación o de comunicación. Implementar un manejo de errores robusto es crucial para mantener la integridad de las aplicaciones y mejorar la experiencia del usuario gestionando de manera elegante los problemas inesperados sin causar cierres abruptos o pérdida de datos.

## Cómo hacerlo:

En VBA, el manejo de errores se implementa típicamente usando la instrucción `On Error` que indica a VBA cómo proceder cuando ocurre un error. Las estrategias de manejo de errores más comunes involucran el `On Error GoTo` etiqueta, `On Error Resume Next` y `On Error GoTo 0`.

**Ejemplo 1: Usando `On Error GoTo`**

Este enfoque te permite dirigir el programa a una sección específica del código, etiquetada inmediatamente después de encontrar un error.

```vb
Sub ErrorHandlerExample()
    On Error GoTo ErrHandler
    Dim intDivision As Integer

    intDivision = 5 / 0 ' Esto causará un error de división por cero

    Exit Sub
ErrHandler:
    MsgBox "Ocurrió un Error: " & Err.Description, vbCritical, "Error!"
    Resume Next
End Sub
```

En este ejemplo, cualquier error en tiempo de ejecución provocará el salto a `ErrHandler`, mostrando un mensaje de error y luego procediendo con la línea siguiente después del error.

**Ejemplo 2: Usando `On Error Resume Next`**

Esta estrategia instruye a VBA a continuar ejecutando la línea de código siguiente incluso si ocurre un error, lo cual puede ser útil para errores que se espera sean inofensivos o cuando planeas manejar el error más adelante en la ejecución.

```vb
Sub ResumeNextExample()
    On Error Resume Next
    Dim intDivision As Integer
    intDivision = 5 / 0 ' Esto no hará que el programa se detenga; el error es ignorado
    
    ' Verificar si ocurrió un error
    If Err.Number <> 0 Then
        MsgBox "Ocurrió un Error: " & Err.Description, vbExclamation, "Error Manejado"
        ' Restablecer el error
        Err.Clear
    End If
End Sub
```

En este caso, el programa no se rompe por un error; verifica si ocurrió un error, lo maneja si fue así, y luego limpia el error.

## Estudio Detallado

Históricamente, el manejo de errores en los lenguajes de programación ha evolucionado desde simples instrucciones goto hasta mecanismos más sofisticados como las excepciones en lenguajes como Java y C#. El manejo de errores de VBA, aunque no tan poderoso o flexible como el manejo moderno de excepciones, cumple su propósito dentro del contexto de la aplicación del lenguaje en la automatización de tareas en entornos de Microsoft Office.

La principal limitación del manejo de errores de VBA radica en su enfoque algo engorroso y manual, requiriendo una colocación cuidadosa del código de manejo de errores y una comprensión clara del flujo de ejecución. Los lenguajes de programación modernos típicamente ofrecen soluciones más elegantes, como los bloques try-catch, que manejan automáticamente el flujo al código de manejo de errores sin necesidad de verificaciones manuales o saltos en la ejecución del código.

A pesar de estas limitaciones, los mecanismos de manejo de errores de VBA son adecuados para la mayoría de las tareas de automatización y, cuando se usan correctamente, pueden reducir significativamente la probabilidad de que los errores no manejados causen problemas para los usuarios. Además, entender el manejo de errores de VBA puede proporcionar insights sobre los paradigmas de programación más antiguos y la evolución de las estrategias de manejo de errores en el desarrollo de software.
