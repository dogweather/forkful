---
title:                "Comprobando si existe un directorio"
html_title:           "Gleam: Comprobando si existe un directorio"
simple_title:         "Comprobando si existe un directorio"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## ¿Por qué?

Imagínate que estás escribiendo un programa para gestionar archivos. Es importante asegurarse de que los archivos se guarden en el lugar correcto y que no se sobrescriban accidentalmente. Una forma de garantizarlo es comprobar si un directorio existe antes de guardar un archivo, y eso es exactamente lo que aprenderemos en este artículo.

## ¿Cómo hacerlo?

En Gleam, existen varias formas de comprobar si un directorio existe. La forma más sencilla es utilizando la función `gleam/os/fs.exists` y pasando la ruta del directorio como argumento. Esta función devolverá `Ok` si el directorio existe y `Error` si no existe.

```Gleam
match os/fs.exists("ruta/al/directorio") {
  Ok(_) -> "El directorio existe.";
  Error(_) -> "El directorio no existe.";
};
```

Otra forma de comprobar si un directorio existe es utilizando la función `gleam/os/fs.read_directory`. Esta función devolverá una lista con los nombres de todos los archivos y directorios que se encuentren en la ruta especificada. Si el directorio no existe, devolverá `Error`.

```Gleam
match os/fs.read_directory("ruta/al/directorio") {
  Ok(lista) -> "Los archivos y directorios dentro del directorio son: {lista}.";
  Error(_) -> "El directorio no existe.";
};
```

## Inmersión profunda

Cuando utilizamos `gleam/os/fs.exists` para comprobar si un directorio existe, la función devuelve `Ok` sin importar si el directorio es accesible o no. Para una comprobación más precisa, podemos utilizar `gleam/os/fs.is_directory` que devuelve `Ok` solo si el directorio existe y es accesible.

```Gleam
match os/fs.is_directory("ruta/al/directorio") {
  Ok(_) -> "El directorio existe y es accesible.";
  Error(_) -> "El directorio no existe o no es accesible.";
};
```

Además, podemos utilizar funciones como `gleam/os/fs.can_read` y `gleam/os/fs.can_write` para verificar si el directorio tiene permisos de lectura y escritura.

## Véase también

- Documentación de Gleam sobre `gleam/os/fs`
- Ejemplos de código de Gleam en GitHub
- Comunidad de Gleam en Discord