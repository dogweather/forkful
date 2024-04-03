---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:10.286382-07:00
description: "C\xF3mo hacerlo: La biblioteca est\xE1ndar de Ruby proporciona m\xE9\
  todos sencillos para verificar la existencia de un directorio. Aqu\xED te mostramos\
  \ c\xF3mo hacerlo\u2026"
lastmod: '2024-03-13T22:44:59.605299-06:00'
model: gpt-4-0125-preview
summary: "La biblioteca est\xE1ndar de Ruby proporciona m\xE9todos sencillos para\
  \ verificar la existencia de un directorio."
title: Comprobando si un directorio existe
weight: 20
---

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
