---
title:    "Haskell: Escribir un archivo de texto"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/haskell/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Por qué escribir un archivo de texto

Escribir un archivo de texto puede ser una tarea muy útil para cualquier programador en Haskell. Además de permitir la persistencia de datos en el tiempo, también es una forma eficiente de compartir información con otros usuarios o sistemas. ¡Aprende cómo hacerlo en este post!

## Cómo hacerlo

Antes de empezar, necesitarás importar el módulo `System.IO` en tu código Haskell. Este módulo contiene las funciones necesarias para trabajar con archivos.

Para escribir un archivo de texto, primero debes abrir un `Handle` utilizando la función `openFile`. Esta función recibe dos parámetros: el nombre del archivo que deseas crear y el modo en el que se abrirá (escritura, lectura, etc).

Una vez que tienes un `Handle`, puedes utilizar la función `hPutStrLn` para escribir una línea de texto en el archivo. Puedes utilizar esta función cuantas veces necesites para escribir todo el contenido deseado en el archivo.

Finalmente, debes cerrar el `Handle` utilizando la función `hclose` una vez que hayas terminado de escribir en el archivo.

Un ejemplo completo de cómo escribir un archivo de texto en Haskell sería el siguiente:

```
```Haskell
import System.IO

main :: IO ()
main = do
    archivo <- openFile "mi_archivo.txt" WriteMode
    hPutStrLn archivo "¡Hola a todos!"
    hPutStrLn archivo "Este es un archivo de texto creado en Haskell."
    hPutStrLn archivo "¡Espero que les guste!"
    hClose archivo
```
```

El resultado de este código sería un archivo de texto llamado "mi_archivo.txt" que contiene las tres líneas de texto especificadas.

## Profundizando

Escribir un archivo de texto puede ser un poco más complejo que solo utilizar las funciones mencionadas anteriormente, ya que pueden surgir algunos problemas como errores al abrir o cerrar el `Handle`, o manejo de excepciones. Sin embargo, con práctica y estudio podrás dominar esta habilidad en no tiempo.

## Ver también

Si quieres aprender más sobre cómo trabajar con archivos en Haskell, te recomendamos revisar la documentación oficial del módulo `System.IO` y practicar con diferentes ejemplos. ¡Buena suerte en tus futuros proyectos! 

* [Documentación oficial del módulo `System.IO`](https://hackage.haskell.org/package/base-4.12.0.0/docs/System-IO.html)
* [Tutorial sobre archivos en Haskell](https://www.fpcomplete.com/haskell/tutorial/io)