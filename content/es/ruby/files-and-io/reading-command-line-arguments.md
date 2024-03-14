---
date: 2024-01-20 17:56:43.852194-07:00
description: "Leer argumentos de la l\xEDnea de comandos en Ruby significa capturar\
  \ datos que los usuarios o scripts externos pasan a tu programa al ejecutarlo. Los\u2026"
lastmod: '2024-03-13T22:44:59.606227-06:00'
model: gpt-4-1106-preview
summary: "Leer argumentos de la l\xEDnea de comandos en Ruby significa capturar datos\
  \ que los usuarios o scripts externos pasan a tu programa al ejecutarlo. Los\u2026"
title: "Lectura de argumentos de l\xEDnea de comandos"
---

{{< edit_this_page >}}

## ¿Qué & Por Qué?
Leer argumentos de la línea de comandos en Ruby significa capturar datos que los usuarios o scripts externos pasan a tu programa al ejecutarlo. Los programadores lo hacen para personalizar la ejecución de sus programas directamente desde el terminal, permitiendo una mayor flexibilidad y control.

## Cómo hacerlo:
```Ruby
# argumentos.rb
puts "Has introducido #{ARGV.length} argumentos:"
puts ARGV
```
Ejecuta el script desde tu consola:
```
$ ruby argumentos.rb estos son 4 argumentos
```

Resultado:
```
Has introducido 4 argumentos:
estos
son
4
argumentos
```

## Profundización
Leer argumentos de la línea de comandos no es nuevo; viene desde los primeros días de los sistemas Unix. En Ruby, `ARGV` es un arreglo especial que almacena esos argumentos. Los scripts de Ruby pueden también usar librerías como `OptionParser` y `Thor` para manejar opciones más complejas.

Antes de que `ARGV` se popularizara, se usaban métodos como la lectura de variables de entorno o archivos de configuración. Aunque `ARGV` es simple y directo, librerías más avanzadas permiten crear interfaces de línea de comandos (CLI) robustas, con validación de argumentos y mensajes de ayuda automatizados.

Los detalles de implementación importantes incluyen:
- `ARGV` contiene solo las cadenas pasadas al programa, sin incluir el nombre del propio script.
- Los elementos se leen como strings, por lo que puedes necesitar convertirlos a otros tipos de datos.
- ARGV es mutable; puedes alterar el arreglo durante la ejecución del programa si es necesario.

## Ver Además
- [Ruby Doc on ARGV](https://ruby-doc.org/core-2.7.0/ARGF.html)
- [OptionParser](https://ruby-doc.org/stdlib-2.6.1/libdoc/optparse/rdoc/OptionParser.html)
