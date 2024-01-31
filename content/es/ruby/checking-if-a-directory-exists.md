---
title:                "Comprobando si existe un directorio"
date:                  2024-01-20T14:58:21.516716-07:00
html_title:           "Gleam: Comprobando si existe un directorio"
simple_title:         "Comprobando si existe un directorio"

category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
Verificar si un directorio existe permite a nuestros programas tomar decisiones inteligentes sobre qué hacer con archivos: leer, escribir, o crear. Es esencial para evitar errores como intentar acceder a un directorio que no está ahí.

## Cómo Hacerlo:
```ruby
# Verificando si existe un directorio con Dir.exist?
if Dir.exist?('/ruta/al/directorio')
  puts '¡El directorio existe!'
else
  puts 'No existe tal directorio.'
end
```
Salida esperada:
```
¡El directorio existe!
```
o
```
No existe tal directorio.
```

## Inmersión Profunda
En los viejos tiempos de Ruby, podrías haber usado `File.exist?` para checar directorios, pero `Dir.exist?` es más explícito y fue introducido en Ruby 1.9.3 para clarificar el código.
Alternativamente, puedes usar `File.directory?('/ruta/al/directorio')` para confirmar que la ruta no solo existe, sino que también es un directorio.
Cabe mencionar que el manejo de excepciones es vital cuando trabajas con archivos y directorios en caso de que los permisos sean un obstáculo o si hay problemas con el sistema de archivos.

## Ver También
- [Documentación oficial de Ruby para Dir.exist?](https://ruby-doc.org/core/Dir.html#method-c-exist-3F)
- [Stack Overflow: Verificando si un directorio existe en Ruby](https://stackoverflow.com/questions/5471032/how-do-i-check-if-a-directory-exists-in-ruby)
- [Ruby-Doc.org para File.directory?](https://ruby-doc.org/core/File.html#method-c-directory-3F)
