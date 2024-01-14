---
title:    "Ruby: Comprobando si existe un directorio"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/ruby/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por qué

La verificación de si un directorio existe es una tarea común en la programación, especialmente en Ruby. Al conocer cómo hacerlo correctamente, podremos asegurarnos de que nuestro código se comporte de la manera que queremos y maneje adecuadamente posibles errores.

## Cómo hacerlo

En Ruby, podemos usar el método `Dir.exist?` para verificar si un directorio existe en una ruta determinada. Este método devuelve `true` si el directorio existe y `false` si no existe. Veamos un ejemplo de cómo podemos usar esto en nuestro código:

```Ruby
# Definimos una ruta
ruta = "/usuarios/juan/documentos"

# Verificamos si el directorio existe
if Dir.exist?(ruta)
  puts "El directorio existe en la ruta especificada."
else
  puts "El directorio no existe en la ruta especificada."
end
```

En este caso, si el directorio "documentos" existe en la ruta `/usuarios/juan`, el código imprimirá "El directorio existe en la ruta especificada." de lo contrario, imprimirá "El directorio no existe en la ruta especificada."

## Profundizando

Además del método `Dir.exist?`, también podemos utilizar `Dir.exists?` para lograr el mismo resultado. Sin embargo, este último está considerado como un método obsoleto y se recomienda usar `Dir.exist?` en su lugar.

Otra forma de verificar la existencia de un directorio es a través del método `File.directory?`. Este método toma como argumento una cadena de texto con la ruta y devuelve `true` si la ruta apunta a un directorio existente y `false` si no es así.

## Ver también

- [File and Directory Operations in Ruby](https://www.sitepoint.com/file-directory-operations-ruby/)
- [How to Check if a File or Directory Exists in Ruby](https://www.rubyguides.com/2018/10/file-directory-exists/)
- [Ruby Dir Class Documentation](https://ruby-doc.org/core-2.7.1/Dir.html)