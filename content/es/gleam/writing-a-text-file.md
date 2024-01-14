---
title:    "Gleam: Escribiendo un archivo de texto"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/gleam/writing-a-text-file.md"
---

{{< edit_this_page >}}

## ¿Por qué escribir un archivo de texto en Gleam?

Escribir un archivo de texto es esencial en cualquier lenguaje de programación, ya que permite almacenar y compartir información de manera organizada y legible. En Gleam, esto se logra utilizando la función `File.write` que permite escribir datos en un archivo especificado.

## ¿Cómo hacerlo?

Para escribir un archivo de texto en Gleam, se sigue el siguiente formato:

```
Gleam
import gleam/file

file.write(
  path: "./mi_archivo.txt",
  data: "¡Hola, mundo!"
)
```
En este ejemplo, se ha importado el módulo `file` y se utiliza la función `write` para especificar la ruta del archivo y los datos que se desean escribir en él. En este caso, se escribe "¡Hola, mundo!" en el archivo `mi_archivo.txt` ubicado en la misma carpeta que el archivo Gleam.

La función `File.write` también permite escribir datos de variables u otros tipos de datos, como por ejemplo:

```
Gleam
import gleam/file

let nombre = "Juan"
let edad = 25
let profesion = "Desarrollador"

let datos = """
Nombre: $(nombre)
Edad: $(edad)
Profesión: $(profesion)
"""

file.write(
  path: "./datos_personales.txt",
  data: datos
)
```

En este ejemplo, se crea una variable `datos` que contiene información personal y se utiliza la función `write` para escribir dichos datos en el archivo `datos_personales.txt`.

## Profundizando

La función `File.write` ofrece más opciones, como por ejemplo especificar el modo de escritura (`append` para agregar datos al final del archivo o `truncate` para sobrescribir el archivo) y configurar permisos de acceso al archivo. Estas opciones pueden consultarse en la documentación oficial de Gleam.

También es importante tener en cuenta que al escribir un archivo, se debe asegurar de cerrarlo correctamente después de su uso utilizando la función `File.close` para evitar posibles errores o pérdida de datos.

## Ver también

- [Documentación oficial de Gleam sobre la función `File.write`](https://gleam.run/documentation/core/modules/file#write)
- [Tutorial de Gleam en español](http://gleam.run/es/tutorials)
- [Ejemplos prácticos de uso de la función `File.write`](https://github.com/gleam-lang/examples/tree/main/file-write)