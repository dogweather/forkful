---
title:                "Comprobando si existe un directorio"
html_title:           "Ruby: Comprobando si existe un directorio"
simple_title:         "Comprobando si existe un directorio"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## ¿Por qué?

Si estás escribiendo un código en Ruby que involucra la manipulación de archivos y directorios, es útil saber si un directorio ya existe antes de crear uno nuevo. Esto te ahorrará tiempo y garantizará que tu código funcione correctamente.

## Cómo hacerlo

Comprobar si un directorio existe en Ruby es muy sencillo. Primero, necesitamos importar la biblioteca "FileUtils" y luego podemos usar el método "exist?" para verificar si un directorio existe en una ubicación específica. Veamos un ejemplo:

```Ruby
require 'fileutils'

if FileUtils.exist?('ruta/al/directorio')
  puts "¡El directorio ya existe!"
else
  puts "El directorio no existe, se creará uno nuevo."
end
```

En este ejemplo, utilizamos la palabra clave "require" para importar la biblioteca "FileUtils", que nos proporciona una variedad de métodos útiles para trabajar con archivos y directorios. Luego, usamos el método "exist?" y le pasamos la ruta del directorio que queremos verificar. Este método devuelve un valor booleano, por lo que podemos usar una declaración "if" para imprimir un mensaje apropiado según el resultado.

## Profundizando

Ahora que sabemos cómo verificar si un directorio existe en Ruby, es importante tener en cuenta algunas cosas adicionales. Por ejemplo, el método "exist?" también puede verificar si un archivo existe en lugar de un directorio, por lo que debes asegurarte de pasar la ruta correcta en tu código. Además, si necesitas realizar operaciones más complejas, como crear o cambiar permisos en un directorio existente, puedes utilizar otros métodos de la biblioteca "FileUtils" como "mkdir" o "chmod".

## Ver también

- [Documentación de la biblioteca FileUtils de Ruby](https://ruby-doc.org/stdlib-2.6.3/libdoc/fileutils/rdoc/FileUtils.html)
- [Comprobación de la existencia de un archivo en Ruby](https://www.rubyguides.com/2018/09/ruby-file-exist/)
- [Manipulación de archivos y directorios con Ruby](https://www.digitalocean.com/community/tutorials/how-to-work-with-file-operations-in-ruby-es)