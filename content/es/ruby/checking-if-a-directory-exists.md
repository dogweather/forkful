---
title:                "Ruby: Comprobando si existe un directorio"
programming_language: "Ruby"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por qué

En programación, a menudo necesitamos verificar si un directorio existe antes de trabajar con él. Esto puede ser útil para asegurarnos de que el directorio requerido esté disponible antes de crear o acceder a archivos dentro de él.

## Cómo hacerlo

La forma más común de verificar si un directorio existe en Ruby es utilizando el método `Dir.exist? ()`. Este método toma una ruta de directorio como argumento y devuelve un valor booleano, `true` si el directorio existe y `false` si no.

Veamos un ejemplo en código para comprender mejor cómo funciona esto:

```Ruby
# Verifique si el directorio "archivos" existe en la carpeta actual
Dir.exist? ("archivos")  #=> true

# Si el directorio no existe, devuelve false
Dir.exist? ("documentos") #=> false
```

También puede combinar este método con `Dir.chdir ()` para cambiar al directorio especificado y luego verificar su existencia:

```Ruby
# Cambiar al directorio "descargas"
Dir.chdir ("descargas")
# Verifique si el directorio "descargas" existe
Dir.exist? (".") #=> true
```

## Profundizando

Además del método `exist? ()`, Ruby también tiene otros métodos para verificar la existencia de un directorio:

- `Dir.empty? ()`: Devuelve `true` si el directorio está vacío y `false` si contiene archivos.
- `Dir.pwd ()`: Devuelve la ruta del directorio actual.
- `File.directory? ()`: Verifica si un archivo es un directorio.
- `Dir.glob ()`: Devuelve una lista de todos los archivos en un directorio que coinciden con un patrón específico.

## Ver también

- Documentación oficial de Ruby sobre el método `Dir.exist? ()`: https://ruby-doc.org/core-2.7.1/Dir.html#method-c-exist-3F
- Tutorial sobre cómo verificar la existencia de un directorio en Ruby: https://www.rubyguides.com/2017/09/ruby-directory/
- Ejemplos de uso de métodos para comprobar la existencia de un directorio en Ruby: https://www.rubydoc.info/stdlib/core/Dir.exist%3F