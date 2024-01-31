---
title:                "Trabajando con archivos CSV"
date:                  2024-01-19
simple_title:         "Trabajando con archivos CSV"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/working-with-csv.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Trabajar con archivos CSV implica manipular datos en un formato de texto sencillo, donde cada línea representa un registro y las comas separan sus campos. Los programadores lo hacen porque es una forma estándar, simple y compatible para intercambiar datos entre distintas aplicaciones y sistemas.

## Cómo hacerlo:

Para manejar archivos CSV en Fish, puedes usar herramientas de línea de comandos como `awk`, `sed` y `cut`. Aquí hay ejemplos:

```fish
# Contar el número de filas en un CSV
awk 'END { print NR }' datos.csv

# Imprimir la segunda columna de un CSV
cut -d ',' -f 2 datos.csv

# Reemplazar comas por punto y coma en un CSV
sed 's/,/;/g' datos.csv > datos_modificados.csv

# Filtrar líneas que contienen 'clave' en un CSV
grep 'clave' datos.csv
```

Output esperado (variará según el contenido de tu archivo CSV):

```plaintext
43
John Doe
```

## Inmersión profunda:

Los CSV no son nuevos. Su uso se remonta a los primeros días de las computadoras personales. Sin embargo, a pesar de formatos de intercambio más modernos como JSON y XML, los CSV permanecen debido a su simplicidad y amplia adopción. Si bien Fish Shell por sí mismo no tiene comandos especializados para CSV, su potencia radica en combinar otras herramientas UNIX. Alternativas como Python con Pandas o programas especializados como `xsv` pueden manejar CSVs más eficientemente en tareas complejas.

## Ver también:

- Documentación de `awk`: https://www.gnu.org/software/gawk/manual/gawk.html
- Tutorial de `sed`: https://www.grymoire.com/Unix/Sed.html
- `cut` en man pages: http://man7.org/linux/man-pages/man1/cut.1.html
- `grep` en man pages: http://man7.org/linux/man-pages/man1/grep.1.html
- `xsv`, para manipulación de CSV más avanzada: https://github.com/BurntSushi/xsv
