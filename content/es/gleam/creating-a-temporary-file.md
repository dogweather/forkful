---
title:    "Gleam: Creando un archivo temporal"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/gleam/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

#¿Por qué crear un archivo temporal en Gleam?

Crear un archivo temporal es una práctica común en la programación, especialmente cuando se trabaja con datos o procesos que requieren un espacio de almacenamiento temporal. En Gleam, esto se puede lograr fácilmente con la función ```Gleam.file.temp()```, que crea un archivo temporal y devuelve su ruta.

## Cómo crear un archivo temporal en Gleam

Para crear un archivo temporal en Gleam, simplemente utilizamos la función mencionada anteriormente y le pasamos como argumento el nombre que queremos darle al archivo. Por ejemplo:

```Gleam
import Gleam.File

file_path = Gleam.File.temp("datos.temp")
```

Este código creará un archivo temporal llamado "datos.temp" y guardará su ruta en la variable ```file_path```.

## Profundizando en la creación de archivos temporales

Cuando se crea un archivo temporal en Gleam, se puede especificar opcionalmente la ruta donde se desea que se guarde el archivo. Si no se proporciona una ruta, el archivo se creará en el directorio de trabajo actual.

Además, es importante tener en cuenta que los archivos temporales creados con la función ```Gleam.File.temp()``` se eliminarán automáticamente cuando el programa termine de ejecutarse. Esto es útil para evitar la acumulación de archivos temporales innecesarios y para mantener un buen uso de los recursos.

# Ver también
- Documentación oficial sobre la función ```Gleam.File.temp()``` (https://gleam.run/modules/gleam/file.html#temp)
- Ejemplos prácticos de uso de archivos temporales en Gleam (https://gleam.run/examples.html)