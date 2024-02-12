---
title:                "Trabajando con CSV"
aliases: - /es/powershell/working-with-csv.md
date:                  2024-02-03T19:20:30.499178-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trabajando con CSV"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/powershell/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## ¿Qué & Por qué?

Trabajar con archivos CSV (Valores Separados por Comas) es una tarea común para administrar y manipular datos de una forma estructurada y tabulada. Los programadores a menudo realizan esta operación para importar, exportar o manipular datos de manera eficiente para varias aplicaciones, tales como análisis de datos, informes o incluso para potenciar aplicaciones web.

## Cómo:

### Leer un Archivo CSV

Para leer desde un archivo CSV, utiliza el cmdlet `Import-Csv`. Este cmdlet lee el archivo y lo convierte en objetos personalizados de PowerShell para cada fila.

```powershell
# Importando un archivo CSV
$data = Import-Csv -Path "C:\Data\users.csv"
# Mostrar el contenido
$data
```

**Salida de muestra:**

```
Name    Age    City
----    ---    ----
John    23     New York
Doe     29     Los Ángeles
```

### Escribir en un Archivo CSV

Por el contrario, para escribir datos en un archivo CSV, se utiliza el cmdlet `Export-Csv`. Este cmdlet toma objetos de entrada y los convierte en un formato CSV.

```powershell
# Creando un objeto para exportar
$users = @(
    [PSCustomObject]@{Name='John'; Age='23'; City='Nueva York'},
    [PSCustomObject]@{Name='Doe'; Age='29'; City='Los Ángeles'}
)

# Exportando a un archivo CSV
$users | Export-Csv -Path "C:\Data\new_users.csv" -NoTypeInformation
```

Después de ejecutar, se crea un archivo llamado `new_users.csv` con los datos proporcionados.

### Filtrar y Manipular el Contenido de un CSV

Para filtrar o manipular los datos de un archivo CSV, utiliza las capacidades de manipulación de objetos de PowerShell. Por ejemplo, para seleccionar solo los usuarios mayores de cierta edad y de una ciudad específica:

```powershell
# Importando y filtrando datos
$filteredData = Import-Csv -Path "C:\Data\users.csv" | Where-Object {
    $_.Age -gt 25 -and $_.City -eq 'Los Ángeles'
}

# Mostrar datos filtrados
$filteredData
```

**Salida de muestra:**

```
Name    Age    City
----    ---    ----
Doe     29     Los Ángeles
```

### Utilizar Bibliotecas de Terceros

Aunque los cmdlets nativos de PowerShell suelen ser suficientes para tareas comunes, operaciones más complejas podrían beneficiarse de bibliotecas o herramientas de terceros. Sin embargo, para la manipulación estándar de CSV, como leer, escribir, filtrar o ordenar, los cmdlets integrados de PowerShell como `Import-Csv` y `Export-Csv` suelen ofrecer una funcionalidad robusta sin necesidad de bibliotecas adicionales.
