---
title:                "Escribiendo en el error estándar"
date:                  2024-01-19
html_title:           "Arduino: Escribiendo en el error estándar"
simple_title:         "Escribiendo en el error estándar"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Qué es y Por Qué?

Escribir en el error estándar permite mostrar mensajes de error y diagnóstico sin mezclarlos con la salida de datos del programa. Es útil para depurar y para que otros programas o usuarios perciban problemas sin confusión.

## Cómo hacerlo:

```Bash
# Escribir un mensaje simple en error estándar
echo "Error: archivo no encontrado" >&2

# Ejemplo de uso con un condicional
if [[ ! -f "archivo.txt" ]]; then
   echo "Error: 'archivo.txt' no existe" >&2
else
   echo "Archivo encontrado: procediendo..."
fi

# Redirigir la salida de error estándar a un archivo
comando_que_falla 2> errores.log

# Ejemplo de salida
# En la consola solo veremos "Archivo encontrado: procediendo..."
# Si 'archivo.txt' no existe, "Error: 'archivo.txt' no existe" se escribirá en errores.log
```

## Deep Dive

Históricamente, la separación entre salida estándar y error estándar viene de las convenciones de Unix y ayuda a manejar ambos flujos independientemente. Alternativas incluyen el uso de herramientas como `logger` para el manejo de registros o el uso de sistemas como `systemd` para manejar servicios y sus salidas de error. Detalles de implementación incluyen redirecciones en Bash (`2>` para STDERR).

## Ver También

- Bash Redirections: https://tldp.org/HOWTO/Bash-Prog-Intro-HOWTO-3.html
- Advanced Bash-Scripting Guide: https://www.tldp.org/LDP/abs/html/
- Bash Manual: https://www.gnu.org/software/bash/manual/
