---
title:    "Haskell: Escribiendo un archivo de texto"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Por qué

Escribir un archivo de texto es una habilidad esencial para cualquier programador de Haskell. Al utilizar archivos de texto, podemos almacenar y acceder a grandes cantidades de datos de manera eficiente, lo que es fundamental en muchas aplicaciones informáticas.

## Cómo hacerlo

Para escribir un archivo de texto en Haskell, primero debemos asegurarnos de importar el módulo System.IO. Luego, podemos utilizar la función writeFile para crear y escribir en un archivo de texto. Veamos un ejemplo:

```Haskell
import System.IO

main = do
    let mensaje = "¡Hola, mundo!"
    writeFile "mi-archivo.txt" mensaje
```

Al ejecutar este código, se creará un archivo de texto llamado "mi-archivo.txt" en el mismo directorio donde se encuentra nuestro archivo Haskell. El contenido del archivo será "¡Hola, mundo!".

Para escribir en archivos de texto ya existentes, podemos utilizar la función appendFile, que agregará el texto al final del archivo en lugar de sobrescribirlo.

## Profundizando

Si queremos tener más control sobre cómo se escriben los archivos de texto, podemos utilizar la función openFile en lugar de writeFile. Esta función nos permite especificar el modo en que se abrirá el archivo, ya sea para escribir, leer o una combinación de ambos. También podemos especificar cómo se manejarán los errores en caso de que ocurran.

Además, podemos utilizar la función hPutStrLn para escribir una línea específica en el archivo, en lugar de escribir todo el contenido de una vez.

## Ver también

- [Documentación del módulo System.IO en Haskell](https://hackage.haskell.org/package/base-4.15.0.0/docs/System-IO.html)
- [Tutorial de Haskell en español](https://haskell-es.github.io/tutorial/)
- [Ejemplos prácticos de escritura de archivos de texto en Haskell](https://wiki.haskell.org/How_to_write_a_Haskell_program)