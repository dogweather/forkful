---
title:                "Leyendo argumentos de línea de comandos"
html_title:           "PowerShell: Leyendo argumentos de línea de comandos"
simple_title:         "Leyendo argumentos de línea de comandos"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/powershell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# ¿Qué y por qué?
Leer los argumentos de la línea de comando es un proceso en el que se recopila información proporcionada por el usuario al momento de ejecutar un script o programa en PowerShell. Los programadores hacen esto para poder personalizar la entrada y el funcionamiento de sus scripts según las necesidades del usuario.

# ¿Cómo hacerlo?
En PowerShell, puedes acceder a los argumentos de la línea de comando utilizando la variable especial `$args`. Esta variable es un array que contiene todos los argumentos pasados por el usuario al momento de ejecutar el script. Puedes utilizar un ciclo `foreach` para recorrer los argumentos y trabajar con ellos según sea necesario.

Ejemplo de código:
```
foreach ($arg in $args) {
    # aquí se puede trabajar con cada argumento
    Write-Host "Argumento recibido: $arg"
}
```

En caso de que necesites trabajar con argumentos que contengan espacios o caracteres especiales, puedes utilizar las comillas dobles (`"`) alrededor del argumento en la línea de comando para que sea leído correctamente por PowerShell.

Ejemplo de código:
```
foreach ($arg in $args) {
    # aquí se puede trabajar con cada argumento
    Write-Host "Argumento con caracteres especiales: $arg"
}
```

# Profundizando
El proceso de lectura de argumentos de línea de comando data de los inicios de la programación en sistemas operativos Unix. En otros lenguajes de programación, como C o Python, se utilizan funciones específicas para acceder a los argumentos de la línea de comando.

Una alternativa a utilizar la variable `$args` en PowerShell es utilizar el módulo `CommandLineParser` de la biblioteca estándar de .NET. Este módulo proporciona una manera más estructurada de trabajar con argumentos de línea de comando y ofrece opciones como argumentos opcionales, valores predeterminados y ayuda para la consola.

# Ver también
- Documentación oficial de Microsoft sobre la variable `$args`: https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_automatic_variable?view=powershell-7
- Documentación oficial de Microsoft sobre el módulo `CommandLineParser`: https://docs.microsoft.com/en-us/dotnet/api/system.commandline.commandlineparser?view=netcore-3.1