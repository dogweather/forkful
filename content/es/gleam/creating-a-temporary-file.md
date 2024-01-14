---
title:                "Gleam: Creando un archivo temporal"
programming_language: "Gleam"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por qué
La creación de un archivo temporal puede ser una tarea común en la programación, ya sea para almacenar datos temporales o para realizar ciertas operaciones que requieren un archivo adicional. En esta publicación de blog, exploraremos cómo crear un archivo temporal en Gleam y por qué puede ser útil.

## Cómo hacerlo
Para crear un archivo temporal en Gleam, utilizaremos la función `temp_file` de la biblioteca `gleam/tempfile`. Comencemos por importar la biblioteca y llamar a la función `temp_file`, pasándole un nombre de archivo como argumento:

```Gleam
import gleam/tempfile

let temp_file = tempfile.temp_file("mi_archivo_temporal")
```

Esto creará un archivo temporal con el nombre especificado y almacenará su ruta en la variable `temp_file`. Ahora que tenemos el archivo, podemos realizar cualquier operación necesaria en él. Al finalizar, es importante borrar el archivo temporal para liberar recursos. Para esto, utilizaremos la función `delete`:

```Gleam
tempfile.delete(temp_file)
```

¡Y eso es todo! Ahora puedes utilizar archivos temporales en tus proyectos de Gleam.

### Ejemplo de código completo
```Gleam
import gleam/tempfile

pub fn main() {
  let temp_file = tempfile.temp_file("mi_archivo_temporal")

  // Realizar operaciones en el archivo temporal...

  tempfile.delete(temp_file)
}
```

### Salida de ejemplo
La salida de este código sería similar a la siguiente:

```
/tmp/mi_archivo_temporal
```

## Profundizando
Cuando creamos un archivo temporal, este se almacena en el sistema de archivos temporal del sistema operativo en el que se está ejecutando nuestro programa. Cada vez que se ejecuta el código que crea el archivo, se genera un nuevo archivo temporal con un nombre único. Esto es útil para evitar conflictos con otros archivos en el sistema y para garantizar que el archivo temporal se elimine correctamente al finalizar el programa.

Además, si necesitamos especificar una ubicación diferente para el archivo temporal, podemos pasar una ruta completa como argumento a la función `temp_file`. Por ejemplo:

```Gleam
let temp_file = tempfile.temp_file("/ruta/mi_archivo_temporal")
```

## Ver también
- Documentación de la biblioteca `gleam/tempfile`: [https://gleam.run/modules/gleam/tempfile/latest](https://gleam.run/modules/gleam/tempfile/latest)
- Tutorial de Gleam en español: [https://gleam.run/es/learn/tutorials/](https://gleam.run/es/learn/tutorials/)