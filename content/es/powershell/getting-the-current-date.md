---
title:                "Obteniendo la fecha actual"
html_title:           "PowerShell: Obteniendo la fecha actual"
simple_title:         "Obteniendo la fecha actual"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/powershell/getting-the-current-date.md"
---

{{< edit_this_page >}}

**¡Obteniendo la Fecha Actual en PowerShell!**

## ¿Qué y por qué?
Obtener la fecha actual es una tarea común para los programadores en diferentes lenguajes de programación, incluyendo PowerShell. Es importante saber la fecha y hora actual en un sistema para realizar tareas como archivar archivos, crear registros de actividad y programar tareas en función del tiempo. Además, la fecha actual es necesaria para cálculos de tiempo y para asegurarse de que el código se está ejecutando correctamente.

## Cómo:
En PowerShell, hay varias formas de obtener la fecha actual. Una forma simple es usar el comando `Get-Date`, que devuelve la fecha y hora actuales en formato de texto legible. Aquí hay un ejemplo de cómo se vería el código y la salida:

```PowerShell
Get-Date
```

Salida:
```bash
lunes, 15 de noviembre de 2021 11:22:30
```

Otra forma de obtener la fecha actual es usando la variable de sistema `$env:DATE`. Esta variable contiene la fecha actual en formato ISO. Aquí hay un ejemplo:

```PowerShell
$env:DATE
```

Salida:
```bash
2021-11-15
```

Además, también puedes personalizar el formato de la fecha y hora utilizando el parámetro `-Format` en el comando `Get-Date`. Por ejemplo, si quieres mostrar solo la hora actual, puedes usar el siguiente código:

```PowerShell
Get-Date -Format "hh:mm:ss tt"
```

Salida:
```bash
11:26:45 AM
```

## Profundizando:
En el pasado, los programadores tenían que escribir código complejo para obtener la fecha actual en sus sistemas. Sin embargo, con PowerShell, obtener la fecha actual se ha vuelto mucho más sencillo. Alternativamente, también puedes obtener la fecha actual mediante el uso de API de Windows en otros lenguajes de programación como C# o VBScript.

Hay varias opciones de formato de fecha y hora disponibles en PowerShell. Puedes consultar la documentación oficial de Microsoft para obtener más detalles sobre cómo personalizar el formato de la fecha y hora.

## Consulta también:
- [Comando Get-Date de PowerShell](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-date?view=powershell-7.2)
- [Detalles sobre formatos de fecha en PowerShell](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-date?view=powershell-7.2#-format)

¡Es así de sencillo obtener la fecha actual en PowerShell! Con estas herramientas, podrás realizar tareas basadas en el tiempo y asegurarte de que tu código se está ejecutando correctamente. Ahora estás listo para seguir aprendiendo y explorando las capacidades de PowerShell. ¡A programar se ha dicho!