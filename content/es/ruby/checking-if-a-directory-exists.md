---
title:                "Verificando si un directorio existe"
html_title:           "Clojure: Verificando si un directorio existe"
simple_title:         "Verificando si un directorio existe"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
Admirada tripulación, comprobar si un directorio existe consiste en verificar, a través de código, si una cierta ubicación en nuestro sistema de archivos existe o no. Esto es vital para prevenir errores al tratar de acceder a un directorio que no está ahí.

## Cómo hacer:
Aquí va el ejemplo, que usa el método `Dir.exist?()`. Escucha esto, si el método devuelve `true`, significa que el directorio existe, si devuelve `false`, significa que no existe.

```Ruby
if Dir.exist?('/ruta/a/tu/directorio')
  puts 'El directorio existe'
else
  puts 'El directorio no existe'
end
```

Si corres este script y el directorio existe, imprimirá `El directorio existe`. Si no existe, imprimirá `El directorio no existe`.

## Inmersión Profunda
Ahora escúchame bien - Ruby no ha tenido siempre el método `Dir.exist?()`. Antes de Ruby 1.9, teníamos que usar el método `File.directory?()`. Sin embargo, el nuevo método es más intuitivo y fácil de recordar. Las alternativas a estos métodos pueden incluir trabajar directamente con la terminal y los comandos del sistema operativo.

Un detalle de implementación a tener en cuenta es que `Dir.exist?()` simplemente envuelve un ligero chequeo del sistema operativo, por lo que es muy rápido y eficiente en comparación con las soluciones personalizadas que intentan leer o abrir el directorio.

## Ver También
Dir.exist? en la documentación oficial de Ruby [aquí](https://ruby-doc.org/core-2.7.1/Dir.html#method-c-exist-3F).
File.directory? en la documentación oficial de Ruby [aquí](https://ruby-doc.org/core-2.7.1/File.html#method-c-directory-3F).