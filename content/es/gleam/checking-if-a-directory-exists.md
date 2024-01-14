---
title:    "Gleam: Comprobando si existe un directorio"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Por qué

A menudo, en la programación, necesitamos verificar si un directorio existe antes de realizar ciertas acciones sobre él. Esto puede ser útil cuando queremos asegurarnos de que el directorio exista antes de crear nuevos archivos o mover archivos a ese directorio. En este artículo, exploraremos cómo podemos verificar de manera eficiente si un directorio existe en Gleam.

## Cómo hacerlo

Para verificar si un directorio existe en Gleam, podemos utilizar la función `File.exists?` del módulo `Gleam.IO.File` y pasarle el path del directorio que queremos verificar como argumento. Si el directorio existe, esta función devolverá `true`, de lo contrario, devolverá `false`.

Veamos un ejemplo de cómo podemos utilizar esta función:

```Gleam
pub fn main() {
  let dir = "/Users/mi_usuario/Documentos/mi_directorio"
  if File.exists?(dir) {
    println!("El directorio existe")
  } else {
    println!("El directorio no existe")
  }
}
```

En este ejemplo, estamos verificando si el directorio `mi_directorio` existe en la ruta especificada. Dependiendo del resultado, imprimirá un mensaje en la consola.

## Profundizando

Recuerda que `File.exists?` solo verifica si el directorio existe o no. Si queremos realizar acciones más complejas, como crear el directorio si no existe o verificar si hay permisos de escritura en el directorio, podemos utilizar otras funciones del módulo `Gleam.IO.File`.

Por ejemplo, podemos utilizar la función `File.stat` para obtener información detallada sobre el directorio, como los permisos, el tamaño y la fecha de creación. También podemos utilizar la función `File.mkdir` para crear el directorio si no existe.

## Ver también

- Documentación oficial de Gleam sobre el módulo `Gleam.IO.File`: https://gleam.run/modules/gleam_io_file.html
- Ejemplos de código para trabajar con directorios en Gleam: https://gist.github.com/gleam-lang/c2d1c7c0953e9c862150a56716f6f3f5