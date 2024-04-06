---
date: 2024-01-20 17:56:43.852194-07:00
description: "C\xF3mo hacerlo: Ejecuta el script desde tu consola."
lastmod: '2024-04-05T21:54:00.964013-06:00'
model: gpt-4-1106-preview
summary: Ejecuta el script desde tu consola.
title: "Lectura de argumentos de l\xEDnea de comandos"
weight: 23
---

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
