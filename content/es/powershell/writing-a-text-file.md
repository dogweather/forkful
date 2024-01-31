---
title:                "Escritura de un archivo de texto"
date:                  2024-01-19
html_title:           "Bash: Escritura de un archivo de texto"
simple_title:         "Escritura de un archivo de texto"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/powershell/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Escribir un archivo de texto en PowerShell permite almacenar datos y resultados de tus scripts. Los programadores lo hacen para registrar información, compartir datos fácilmente y automatizar tareas.

## Cómo Hacerlo:

Puedes usar el cmdlet `Out-File` para escribir un archivo de texto:

```PowerShell
"¡Hola, PowerShell!" | Out-File -FilePath .\Salida.txt
```
Salida esperada (contenido del archivo `Salida.txt`):

```
¡Hola, PowerShell!
```

Otro ejemplo utilizando `Set-Content`:

```PowerShell
Set-Content -Path .\Salida.txt -Value "Registro de actividades"
```

## Deep Dive:

Antes, en el mundo de PowerShell v1.0 y v2.0, `Out-File` era frecuentemente el método predilecto para escribir archivos de texto. Ahora, con diferentes cmdlets como `Set-Content` y `Add-Content`, tienes más control sobre cómo se escribe en los archivos. `Set-Content` reemplaza el contenido existente, mientras que `Add-Content` agrega al final. Importante notar que `Out-File` es ideal para la salida desde la tubería, mientras `Set-Content` trabaja mejor para cadenas y colecciones directas.

## Ver También:

- [Set-Content documentation](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.management/set-content?view=powershell-7)
- [Out-File documentation](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/out-file?view=powershell-7)
- [Add-Content documentation](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.management/add-content?view=powershell-7)
