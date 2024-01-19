---
title:                "Iniciando un nuevo proyecto"
html_title:           "Bash: Iniciando un nuevo proyecto"
simple_title:         "Iniciando un nuevo proyecto"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/powershell/starting-a-new-project.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Empezar un nuevo proyecto significa idear y desarrollar una nueva aplicación o software desde cero. Los programadores lo hacen para resolver problemas únicos, para aprender nuevas habilidades, o para crear algo innovador.

## ¿Cómo hacerlo?

Aquí te muestro cómo puedes iniciar un nuevo proyecto en PowerShell.

```PowerShell
# Crea una nueva carpeta para tu proyecto
New-Item -ItemType "directory" -Path "C:\myProject

# Cambia al directorio de tu proyecto
cd C:\myProject 

# Crea un nuevo archivo .ps1
New-Item -ItemType "file" -Name "main.ps1"
```
Después de ejecutar este código, deberías ver una salida como esta:

```PowerShell
Directory: C:\

Mode                LastWriteTime         Length Name                                                                  
----                -------------         ------ ----                                                                  
d-----       24/10/2021  12:34 PM                myProject
```

```PowerShell
Directory: C:\myProject

Mode                LastWriteTime         Length Name                                                                  
----                -------------         ------ ----                                                                  
-a----       24/10/2021  12:35 PM              0 main.ps1
```

## Profundización

Históricamente, empezar un nuevo proyecto solía requerir más planificación y tiempo porque los recursos de computación y código eran más costosos. Con la llegada de lenguajes de script como PowerShell y el rápido progreso en la tecnología de las computadoras, esto se ha vuelto más fácil y ágil.

En PowerShell, puedes alternativamente usar la cmdlet `Out-File` para crear archivos. Si bien `New-Item` es más directa, `Out-File` puede ser útil si necesitas redirigir la salida de un comando a un archivo.

```PowerShell
Get-Process | Out-File -FilePath "C:\myProject\processes.txt"
```

El comando que se acaba de compartir generará un archivo processes.txt con una lista de todos los procesos en ejecución actuales.

## Ver también

- [Documentación oficial de PowerShell](https://docs.microsoft.com/en-us/powershell/)
- [PowerShell: Crear, leer, actualizar, eliminar archivos](https://ps1.guru/powershell-files/)
- [Tutorial de PowerShell para principiantes](https://www.tutorialspoint.com/powershell/index.htm)