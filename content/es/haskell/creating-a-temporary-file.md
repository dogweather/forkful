---
title:    "Haskell: Creando un archivo temporal"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/haskell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por qué crear un archivo temporal en Haskell?

La creación de archivos temporales es una práctica común en la programación para almacenar datos de manera temporal. En Haskell, los archivos temporales pueden ser útiles para guardar información durante la ejecución del programa y luego ser eliminados.

## Cómo crear un archivo temporal en Haskell

Crear un archivo temporal en Haskell es bastante sencillo usando la función `openTempFile`. Esta función toma dos parámetros: un directorio de trabajo y una cadena de texto que servirá como prefijo para el nombre del archivo temporal.

```Haskell
import System.IO

main = do
  (tempFile, tempHandle) <- openTempFile "." "temp"

  hPutStrLn tempHandle "¡Hola, mundo!"
  hClose tempHandle

  putStr "Se ha creado el archivo temporal en: "
  putStrLn tempFile
```

El código anterior primero importa el módulo `System.IO` que contiene funciones para manejar archivos. Luego, en la función `main`, utilizamos `openTempFile` para crear un archivo temporal en el directorio actual con el prefijo "temp". Luego escribimos una cadena en el archivo temporal utilizando la función `hPutStrLn` y finalmente cerramos el archivo con `hClose`. Por último, mostramos la ubicación del archivo temporal en la consola.

La salida del programa sería:

```
Se ha creado el archivo temporal en: ./temp8792.tmp
```

## Profundizando en la creación de archivos temporales

En Haskell, la función `openTempFile` realmente genera un archivo con un nombre único y devuelve su ubicación y un manejador de archivo. Los archivos temporales generados de esta manera se eliminan automáticamente una vez que se cierra el manejador de archivo. Esto significa que no tenemos que preocuparnos por eliminar explícitamente los archivos temporales después de su uso.

También podemos usar la función `openBinaryTempFile` para crear archivos temporales con datos binarios en lugar de texto.

## Ver también

- [Documentación de `System.IO`](https://hackage.haskell.org/package/base/docs/System-IO.html)
- [Tutorial de Haskell en español](https://www.haskell.org/learn/)
- [Haskell en 5 minutos](https://www.youtube.com/watch?v=6MXu2Y_r4mE)