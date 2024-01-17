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

## ¿Qué & Por qué?

Comprobar si un directorio existe es una acción común en la programación. Básicamente, es una forma de verificar si una determinada carpeta o directorio está presente en un sistema de archivos. Los programadores suelen hacer esto para asegurarse de que los archivos o recursos que necesitan estén disponibles antes de seguir adelante con su código.

## Cómo:

Para comprobar si un directorio existe en Ruby, podemos usar el método `Dir.exist?`seguido por el nombre del directorio que queremos verificar. Por ejemplo:

```Ruby
Dir.exist?("mi_directorio")
```

El resultado de este código será un valor booleano `true` o `false`, dependiendo de si el directorio existe o no.

## Inmersión Profunda:

La verificación de la existencia de un directorio ha sido una tarea común en la programación durante mucho tiempo. En el pasado, los programadores solían usar el comando `ls` (en sistemas Unix/Linux) o `dir` (en sistemas Windows) para listar los archivos y directorios en un sistema de archivos y luego buscar el nombre del directorio en esa lista. Sin embargo, esto no era muy eficiente ya que podía ser lento si el directorio estaba en un directorio profundo o si había muchos archivos en el sistema.

Otra alternativa es usar el comando `File.exist?`, que también puede verificar la existencia de un directorio (aunque también puede usarse para verificar la existencia de un archivo). Sin embargo, el método `Dir.exist?` es más específico y recomendado para este propósito.

En cuanto a la implementación, el método `Dir.exist?` usa llamadas al sistema para verificar si el directorio existe. Esto significa que es una forma más rápida y eficiente de comprobar la existencia de un directorio en comparación con el uso de comandos externos como `ls` o `dir`.

## Ver también:

- [Documentación de Ruby sobre el método Dir.exist?](https://ruby-doc.org/core-2.7.1/Dir.html#method-c-exist-3F)
- [Método File.exist? para verificar la existencia de archivos](https://ruby-doc.org/core/Dir.html#method-c-exist-3F)
- [Stack Overflow: Check if a directory exists in Ruby](https://stackoverflow.com/questions/8256079/check-if-a-directory-exists-in-ruby)