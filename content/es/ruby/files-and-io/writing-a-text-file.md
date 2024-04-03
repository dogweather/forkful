---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:28:48.458931-07:00
description: "C\xF3mo hacerlo: Ruby simplifica las operaciones con archivos. Para\
  \ escribir en un archivo, puedes usar la clase incorporada de Ruby `File`. El siguiente\u2026"
lastmod: '2024-03-13T22:44:59.609149-06:00'
model: gpt-4-0125-preview
summary: Ruby simplifica las operaciones con archivos.
title: Escribiendo un archivo de texto
weight: 24
---

## Cómo hacerlo:
Ruby simplifica las operaciones con archivos. Para escribir en un archivo, puedes usar la clase incorporada de Ruby `File`. El siguiente ejemplo demuestra cómo abrir un archivo para escribir (modo `"w"`) y añadir (modo `"a"`), luego escribir una cadena en él y asegurar que el archivo se cierre después:

```ruby
# Escribiendo nuevo contenido en un archivo, sobrescribiendo el contenido existente
File.open("example.txt", "w") do |file|
  file.puts "Hola, Ruby!"
end

# Añadiendo contenido al final de un archivo
File.open("example.txt", "a") do |file|
  file.puts "Añadiendo otra línea."
end
```
Después de ejecutar ambos fragmentos, el contenido de `example.txt` será:
```
Hola, Ruby!
Añadiendo otra línea.
```

### Usando una biblioteca de terceros: FileUtils
Para operaciones de archivos más complejas, la biblioteca estándar de Ruby `FileUtils` puede ser muy útil, aunque para la escritura de archivos básicos, los métodos estándar de `File` son suficientes. Sin embargo, si quieres copiar, mover, eliminar o realizar otras operaciones del sistema de archivos en conjunto con la escritura de archivos, `FileUtils` vale la pena explorar.

Un ejemplo de uso de `FileUtils` para crear un directorio y luego escribir en un archivo dentro de ese directorio:
```ruby
require 'fileutils'

FileUtils.mkdir_p 'logs'
File.open("logs/hoy.log", "w") do |file|
  file.puts "Entrada de registro: #{Time.now}"
end
```

Esto demuestra la creación de un nuevo directorio `logs` si aún no existe, y la escritura en un nuevo archivo `hoy.log` dentro de él, mostrando tanto la manipulación de directorios como de archivos sin escribir directamente con FileUtils, pero utilizando su capacidad de manejo de directorios.
