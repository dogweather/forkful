---
title:                "Trabajando con csv"
html_title:           "Fish Shell: Trabajando con csv"
simple_title:         "Trabajando con csv"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/working-with-csv.md"
---

{{< edit_this_page >}}

## ¿Qué y Por qué?

Trabajar con CSV (valores separados por comas) es una forma común de manejar datos tabulares en programación. Los programadores lo hacen para leer, escribir y manipular datos en un formato estructurado de manera eficiente.

## Cómo hacerlo:

El siguiente es un ejemplo de cómo trabajar con un archivo CSV en Fish Shell:

```Fish Shell

# Importar módulo CSV
set csv (busca --path --qual fz)

# Crear un nuevo archivo CSV
set nuevo_csv "nuevo_archivo.csv"

# Escribir datos en el archivo
for i in (seq 1 3)
  set filas (seq 1 5)
  for j in $filas
    echo "Fila $j, Columna $i" >> $nuevo_csv
  end
end

# Leer el archivo CSV
set leido_csv (csv $nuevo_csv)

echo $leido_csv

# Resultado:
# Fila 1, Columna 1
# Fila 2, Columna 1
# Fila 3, Columna 1
# Fila 4, Columna 1
# Fila 5, Columna 1
# Fila 1, Columna 2
# Fila 2, Columna 2
# Fila 3, Columna 2
# Fila 4, Columna 2
# Fila 5, Columna 2
# Fila 1, Columna 3
# Fila 2, Columna 3
# Fila 3, Columna 3
# Fila 4, Columna 3
# Fila 5, Columna 3

```

## Inmersión profunda:

El formato CSV se remonta a los años 1970 cuando era común almacenar datos en hojas de cálculo. Alternativas a trabajar con CSV incluyen convertir los datos a otros formatos, como JSON o YAML, o utilizar bases de datos relacionales. Fish Shell ofrece el módulo CSV para trabajar con CSV de manera fácil y rápida. Este módulo utiliza el programa "busca" para interpretar los archivos CSV y permite leer y escribir en formato de tabla.

## Ver también:

- Página oficial de Fish Shell: https://fishshell.com/
- Documentación del módulo CSV en Fish Shell: https://fishshell.com/docs/current/cmds/busca.html
- Más información sobre el formato CSV: https://es.wikipedia.org/wiki/Comma-separated_values