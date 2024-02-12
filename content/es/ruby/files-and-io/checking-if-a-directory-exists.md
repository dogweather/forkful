---
title:                "Comprobando si un directorio existe"
aliases:
- /es/ruby/checking-if-a-directory-exists.md
date:                  2024-02-03T19:08:10.286382-07:00
model:                 gpt-4-0125-preview
simple_title:         "Comprobando si un directorio existe"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Comprobar si un directorio existe en Ruby permite a los programadores verificar la presencia de un directorio antes de realizar operaciones como leer archivos o crear nuevos directorios. Esto es crucial para evitar errores en el manejo de archivos y asegurar la fiabilidad de las manipulaciones del sistema de archivos.

## Cómo hacerlo:
La biblioteca estándar de Ruby proporciona métodos sencillos para verificar la existencia de un directorio. Aquí te mostramos cómo hacerlo con Ruby puro, sin necesidad de bibliotecas de terceros:

```ruby
require 'fileutils'

# Comprobar si un directorio existe
if Dir.exist?('/ruta/al/directorio')
  puts 'El directorio existe.'
else
  puts 'El directorio no existe.'
end
```
Salida de muestra:
```
El directorio existe.
```
O:
```
El directorio no existe.
```

Además de usar `Dir.exist?`, también puedes utilizar el método `File.directory?` que devuelve `true` si la ruta dada es un directorio:

```ruby
if File.directory?('/ruta/al/directorio')
  puts 'El directorio existe.'
else
  puts 'El directorio no existe.'
end
```
Tanto `Dir.exist?` como `File.directory?` forman parte de la biblioteca estándar de Ruby y no requieren de gemas externas para su uso, lo que los hace opciones convenientes y eficientes para la verificación de directorios.
