---
title:                "Trabajando con archivos CSV"
date:                  2024-01-19
html_title:           "Bash: Trabajando con archivos CSV"
simple_title:         "Trabajando con archivos CSV"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/working-with-csv.md"
---

{{< edit_this_page >}}

## Qué es y por qué?
Trabajar con CSV (valores separados por comas) significa manipular archivos de texto que almacenan tablas de datos, cada línea un registro y cada registro dividido en campos por comas. Los programadores lo usan por su simplicidad y compatibilidad universal para importar, exportar y manipular datos.

## Cómo hacerlo:
Vamos directo al grano con algunos ejemplos de manipulación de CSV en Bash.

```Bash
# Leer y mostrar contenido de un archivo CSV
cat datos.csv

# Contar el número de filas en un archivo CSV
wc -l < datos.csv

# Mostrar la primera columna de todos los registros
cut -d',' -f1 datos.csv

# Añadir una columna al final de cada fila
awk -F',' '{print $0",nueva_columna"}' datos.csv

# Filtrar filas que contienen 'ejemplo'
grep 'ejemplo' datos.csv

# Ordenar el contenido de un archivo CSV según la segunda columna
sort -t',' -k2 datos.csv
```
Ejemplo de salida para el comando `cut`:
```
id_usuario
123
456
789
```

## Más detalles:
CSV es el formato de intercambio de datos más simple, concebido en los inicios de la informática personal cuando se buscaba una forma eficiente de mover datos en aplicaciones como bases de datos y hojas de cálculo. A pesar de su simplicidad, hay alternativas como JSON o XML, que proporcionan estructuras más complejas y metadatos. Sin embargo, CSV sigue siendo relevante por su facilidad de uso y amplia aceptación. Implementar scripts de Bash para manipular CSV es directo, pero se complica con datos complejos o cuando se requieren acciones más avanzadas, para lo cual se podrían considerar herramientas como `csvkit`.

## Ver también:
Para ampliación y herramientas adicionales consulta los siguientes enlaces:

- GNU `coreutils`: https://www.gnu.org/software/coreutils/manual/coreutils.html
- `awk`: https://www.gnu.org/software/gawk/manual/gawk.html
- `csvkit`: https://csvkit.readthedocs.io/en/latest/
- Bash scripting guide: https://tldp.org/LDP/Bash-Beginners-Guide/html/
