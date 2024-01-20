---
title:                "Verificando si un directorio existe"
html_title:           "Elm: Verificando si un directorio existe"
simple_title:         "Verificando si un directorio existe"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/powershell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

---

# Verificar si un directorio existe en PowerShell

La ejecución de scripts y funciones puede depender de la existencia de directorios y archivos en nuestros sistemas. Aquí es donde PowerShell impone su utilidad y precisión.

## ¿Qué y Por Qué?

Verificar si un directorio existe es una tarea común en la programación para prevenir errores al tratar con archivos y directorios que pueden o no estar presentes. Los programadores lo hacen para asegurar que los directorios existen antes de realizar operaciones en ellos y para crearlos si no están presentes.

## Cómo hacerlo:

PowerShell lo simplifica con el cmdlet `Test-Path`:

```PowerShell
# Asegurarse si un directorio existe
$dir = "C:\mi\directorio"

if (Test-Path $dir)
{
    Write-Output "El directorio existe"
}
else
{
    Write-Output "El directorio no existe"
}
```

Resultado de muestra:

```PowerShell
El directorio existe
```

Si desea crear el directorio si no existe, puede hacerlo así:

```PowerShell
# Crear un directorio si no existe
$dir = "C:\mi\directorio"

if (!(Test-Path $dir))
{
    New-Item -Path $dir -ItemType Directory
}
```

## Inmersión Profunda:

1. **Contexto histórico:** PowerShell, disponible por primera vez en noviembre de 2006, ha simplificado muchas tareas de administración y programación en Windows con su enfoque orientado a objetos.

2. **Alternativas:** Antes de PowerShell, uno solía verificar la existencia de un directorio con scripts de lotes usando `IF EXIST`.

3. **Detalles de implementación:** `Test-Path` devuelve un booleano, verdadero si el directorio existe y falso si no. `New-Item`, a su vez, será invocado si `Test-Path` devuelve falso, creando así el directorio.

## Ver También:

- [Documentos Oficiales de Microsoft PowerShell](https://docs.microsoft.com/es-es/powershell/)
- [Comunidad de Desarrolladores de PowerShell - Stack Overflow](https://stackoverflow.com/questions/tagged/powershell)

---