---
title:                "Ruby: Comprobando si existe un directorio"
simple_title:         "Comprobando si existe un directorio"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

##Por qué

Hay muchas razones por las cuales alguien puede querer verificar si un directorio existe en Ruby. Puede ser para asegurarse de que un archivo pueda ser guardado correctamente en un lugar específico, o para confirmar la existencia de un directorio antes de realizar una acción en él.

##Cómo hacerlo

Verificar si un directorio existe en Ruby es bastante fácil. Primero, necesitamos importar la clase `FileUtils` para poder usar su método `mkdir` para crear un directorio, si no existe. Luego, usamos el método `exist?` para verificar si el directorio existe o no. A continuación, un ejemplo de cómo hacerlo:

```
require 'fileutils'

# Crear un directorio llamado "proyectos" en la carpeta actual
FileUtils.mkdir("proyectos")

# Verificar si el directorio "proyectos" existe
if FileUtils.exist?("proyectos")
  puts "El directorio proyectos existe."
else
  puts "El directorio proyectos no existe."
end
```

El programa mostrará la siguiente salida:

```
El directorio proyectos existe.
```

##Profundizando

Para aquellos que quieran saber más sobre cómo funciona la verificación de directorios en Ruby, aquí hay algunos detalles adicionales a tener en cuenta. En primer lugar, como se mencionó anteriormente, utilizamos el método `exist?` de la clase `FileUtils` para verificar la existencia de un directorio. Este método devuelve un valor booleano, `true` si el directorio existe y `false` si no existe.

También es importante tener en cuenta que `exist?` no solo se aplica a directorios, sino también a archivos. Por lo tanto, es importante ser específico en nuestra ruta al verificar la existencia de un directorio para evitar falsos positivos.

En caso de que necesitemos crear un directorio que no exista, podemos usar el método `mkdir_p` en lugar de simplemente `mkdir`. Esto asegurará que si el directorio ya existe, no se genere un error y si no existe, se creará correctamente.

##Ver también

- [Ruby Docs: FileUtils](https://ruby-doc.org/stdlib-2.7.2/libdoc/fileutils/rdoc/FileUtils.html)
- [The Ruby Toolbox: FileUtils](https://www.ruby-toolbox.com/projects/fileutils)
- [Ejemplo de verificación de directorios en Ruby](https://www.rubyguides.com/2015/05/working-with-files-ruby/)