---
title:                "Trabajando con csv"
html_title:           "PowerShell: Trabajando con csv"
simple_title:         "Trabajando con csv"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/powershell/working-with-csv.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Trabajar con CSV (Comma Separated Values) es una tarea común para los programadores que necesitan manipular datos en formato tabular. Los archivos CSV son fáciles de crear y leer, lo que los hace populares para almacenar y compartir datos entre diferentes aplicaciones.

## ¿Cómo hacerlo?
En PowerShell, podemos utilizar los cmdlets Import-Csv y Export-Csv para trabajar con archivos CSV.

```PowerShell
# Importar un archivo CSV en una variable 
$datos = Import-Csv "C:\ruta\archivo.csv"

# Exportar una variable a un archivo CSV
$datos | Export-Csv "C:\ruta\nuevo_archivo.csv"
```

Podemos agregar opciones adicionales al cmdlet Export-Csv para controlar el formato de los datos, como por ejemplo, el delimitador y la codificación del archivo de salida.

```PowerShell
# Exportar una variable a un archivo CSV con delimitador punto y coma
$datos | Export-Csv "C:\ruta\nuevo_archivo.csv" -Delimiter ";"

# Exportar una variable a un archivo CSV con formato UTF-8
$datos | Export-Csv "C:\ruta\nuevo_archivo.csv" -Encoding "UTF8"
```

## Inmersión profunda
La historia detrás de los archivos CSV se remonta a la época de las primeras hojas de cálculo, cuando se utilizaban comas como separadores de datos. A medida que las tecnologías avanzaron, los archivos CSV se volvieron obsoletos, pero aún se mantienen en uso debido a su simplicidad y compatibilidad con una amplia gama de aplicaciones.

En cuanto a las alternativas, PowerShell también ofrece cmdlets para trabajar con otros formatos de datos tabulares, como XML y JSON. Sin embargo, los archivos CSV siguen siendo una opción popular debido a su facilidad de uso.

Para aquellos interesados en conocer más detalles sobre el formato CSV, se recomienda revisar la especificación RFC 4180, que describe los estándares para crear archivos CSV correctamente.

## Ver también
- [Documentación oficial de Microsoft para Import-Csv](https://docs.microsoft.com/es-es/powershell/module/microsoft.powershell.utility/import-csv)
- [Documentación oficial de Microsoft para Export-Csv](https://docs.microsoft.com/es-es/powershell/module/microsoft.powershell.utility/export-csv)
- [RFC 4180 - Formato de archivo CSV](https://tools.ietf.org/html/rfc4180)