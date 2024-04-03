---
date: 2024-01-20 17:55:38.320708-07:00
description: "C\xF3mo hacerlo: Para probar, guarda el siguiente c\xF3digo en un archivo\
  \ llamado `lectura_args.sh`."
lastmod: '2024-03-13T22:44:59.262478-06:00'
model: gpt-4-1106-preview
summary: "Para probar, guarda el siguiente c\xF3digo en un archivo llamado `lectura_args.sh`."
title: "Lectura de argumentos de l\xEDnea de comandos"
weight: 23
---

## Cómo hacerlo:
Para probar, guarda el siguiente código en un archivo llamado `lectura_args.sh`.

```Bash
#!/bin/bash
# Usar $1 para acceder al primer argumento, $2 para el segundo, etc.
echo "El primer argumento es: $1"
echo "El segundo argumento es: $2"

# Usar $@ para acceder a todos los argumentos como una lista
echo "Todos los argumentos: $@"

# Usar $# para contar los argumentos pasados
echo "Número de argumentos pasados: $#"
```

Dale permisos de ejecución al script y correlo:

```Bash
chmod +x lectura_args.sh
./lectura_args.sh hola mundo
```

Salida esperada:

```
El primer argumento es: hola
El segundo argumento es: mundo
Todos los argumentos: hola mundo
Número de argumentos pasados: 2
```

## Profundización:
Históricamente, los argumentos de línea de comandos se remontan a los primeros días de Unix. Son una forma estándar de interactuar con programas en la terminal. Aparte de `$1`, `$2`, `...`, `$@`, y `$#`, existen otras variables especiales como `$0`, que es el nombre del script mismo. También encontrarás que `getopts` y `getopt` son herramientas más avanzadas para manejar argumentos nombrados o “flags”.

### Alternativas:
- `getopts`: Lo usas en scripts más complejos donde los argumentos tienen opciones y banderas.
- `getopt`: Similar a `getopts`, pero es una versión externa más antigua y no se recomienda usar debido a problemas con la portabilidad y la seguridad.

### Detalles de Implementación:
- Puedes iterar sobre `$@` con un loop para hacer algo con cada argumento.
- Si un argumento contiene espacios, úsalo entre comillas para que el script lo lea como un solo valor.

## Ver También:
- Guía avanzada de Bash: http://www.tldp.org/LDP/abs/html/
- Manual de Bash: https://www.gnu.org/software/bash/manual/
- Tutorial de getopts: https://wiki.bash-hackers.org/howto/getopts_tutorial
