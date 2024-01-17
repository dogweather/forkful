---
title:                "Creando un archivo temporal"
html_title:           "Gleam: Creando un archivo temporal"
simple_title:         "Creando un archivo temporal"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# ¿Qué y por qué?

La creación de un archivo temporal en la programación es una práctica común que consiste en generar un archivo temporal para almacenar datos de forma temporal mientras un programa está en ejecución. Los programadores lo hacen para evitar la sobrecarga de almacenar datos en memoria y para tener un espacio dedicado para almacenarlos de manera temporal.

# Cómo hacerlo:

Para crear un archivo temporal en Gleam, puedes utilizar la función ```Gleam.FileSystem.Temp.create()```, que creará un archivo temporal en la ubicación predeterminada del sistema operativo. Este archivo tendrá un nombre generado automáticamente y podrás acceder a él a través de la estructura de datos que devuelve la función.

Ejemplo de código:

```
funcion main() {
  archivo_temp = Gleam.FileSystem.Temp.create()
  GleamIO.print("Archivo temporal creado: ")
  GleamIO.print(archivo_temp.path)
}
```

Salida:

```
Archivo temporal creado: /tmp/glmMxkNgTH
```

# Buceo profundo:

La creación de archivos temporales es una práctica que se remonta a los inicios de la programación, cuando las computadoras tenían una capacidad limitada de almacenamiento en memoria. Hoy en día, sigue siendo una práctica útil para programas que manejan grandes cantidades de datos.

Además de la función ```create()```, existen otras formas de crear archivos temporales en Gleam, como utilizando la biblioteca ```GleamFsTmp``` o pasando una ubicación personalizada a la función ```create()```.

Otras alternativas a la creación de archivos temporales incluyen el uso de bases de datos o el almacenamiento de datos en la nube. Sin embargo, la creación de archivos temporales sigue siendo una opción eficiente y sencilla para almacenar datos de manera temporal.

# Ver también:

- Documentación de la función ```create()```: https://gleam.run/modules/gleamfs/tmp.html#create
- Biblioteca GleamFsTmp: https://github.com/gleam-lang/gleam_fs_tmp