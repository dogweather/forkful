---
title:                "Trabajando con archivos CSV"
date:                  2024-01-19
html_title:           "Bash: Trabajando con archivos CSV"
simple_title:         "Trabajando con archivos CSV"

category:             "PowerShell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/powershell/working-with-csv.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Trabajar con CSV (Valores Separados por Comas) es manejar datos estructurados como texto plano, ideal para importar y exportar información de aplicaciones. Programadores lo usan por su simplicidad y compatibilidad universal.

## Cómo hacerlo:
```PowerShell
# Importar CSV
$datos = Import-Csv -Path "datos.csv"

# Mostrar los datos importados
$datos

# Añadir una nueva fila
$nuevaFila = [PSCustomObject]@{
    Nombre = "Ana"
    Edad = 28
    Ciudad = "Sevilla"
}
$datos += $nuevaFila

# Exportar a CSV
$datos | Export-Csv -Path "datos_actualizados.csv" -NoTypeInformation

# Verificar el contenido del nuevo CSV
Get-Content "datos_actualizados.csv"
```

**Salida de ejemplo:**
```
"Nombre","Edad","Ciudad"
"Juan","30","Madrid"
"Lucia","25","Barcelona"
"Ana","28","Sevilla"
```

## Inmersión Profunda
Históricamente, CSV se popularizó por su facilidad de edición con programas de hoja de cálculo. Las alternativas incluyen JSON y XML, que ofrecen estructuras más complejas para datos anidados. En PowerShell, trabajar con CSV es directo gracias a `Import-Csv` y `Export-Csv`, que convierten entre objetos de PowerShell y texto CSV.

## Ver También
- La documentación oficial de PowerShell para `Import-Csv`: https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/import-csv
- La documentación oficial de PowerShell para `Export-Csv`: https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/export-csv
- Tutorial sobre cómo manipular CSV con PowerShell: https://ss64.com/ps/syntax-csv.html
