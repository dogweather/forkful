---
title:                "Leyendo argumentos de la línea de comandos"
html_title:           "Bash: Leyendo argumentos de la línea de comandos"
simple_title:         "Leyendo argumentos de la línea de comandos"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/powershell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## ¿Qué y Por qué?

La lectura de argumentos de línea de comandos se refiere a cómo un programa de línea de comandos accede a los datos que un usuario proporciona cuando se ejecuta el programa. Los programadores lo hacen para interactuar dinámicamente con los datos del usuario sin la necesidad de cambiar y recompilar el código del programa.

## Cómo hacer:

Para acceder a los argumentos de línea de comandos en PowerShell, usamos la variable automática `$args` que contiene una matriz de los argumentos no analizados. Aquí hay un ejemplo:

```PowerShell
# Argumentos.ps1
Write-Host "Número de argumentos: $($args.Count)"
for ($i=0; $i -lt $args.Count; $i++)
{
    Write-Host "Argumento $i es: $($args[$i])"
}
```

Ahora, si ejecutamos el script `Argumentos.ps1` con algunos argumentos como:

```PowerShell
PS C:\> .\Argumentos.ps1 uno dos tres
```

Obtendríamos la siguiente salida:

```PowerShell
Número de argumentos: 3
Argumento 0 es: uno
Argumento 1 es: dos
Argumento 2 es: tres
```

## Visión Detallada:

Desde una perspectiva histórica, la lectura de argumentos de línea de comandos ha sido una característica básica en la programación de línea de comandos desde los primeros días de Unix.

Como alternativa a `$args`, PowerShell ofrece funciones avanzadas que pueden declarar parámetros para tomar entrada. Esto ofrece una manera más formateada y flexible de gestionar la entrada del usuario.

En cuanto a los detalles de implementación, `$args` es una matriz, como tal, puede utilizar métodos de matriz en `$args`. Sin embargo, uno tiene que considerar que `$args` es solo útil en el ámbito del script actual y no se pasa a los bloques de comando hijos.

## Ver También:

- Documentación de Microsoft sobre `$args`: https://docs.microsoft.com/es-es/powershell/module/microsoft.powershell.core/about/about_automatic_variables?view=powershell-7.1#args
- Información detallada sobre funciones avanzadas: https://docs.microsoft.com/es-es/powershell/module/microsoft.powershell.core/about/about_functions_advanced?view=powershell-7.1
- Información sobre el uso del `$args` en bloques de comando secuenciales: https://stackoverflow.com/questions/33115019/powershell-pass-args-into-other-scripts