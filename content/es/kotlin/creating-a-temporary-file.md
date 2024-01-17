---
title:                "Creación de un archivo temporal"
html_title:           "Kotlin: Creación de un archivo temporal"
simple_title:         "Creación de un archivo temporal"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
La creación de un archivo temporal en Kotlin es una práctica común entre los programadores. Se trata de crear un archivo que se eliminan automáticamente después de su uso. Esto se hace por varias razones, como mejorar la seguridad de la aplicación, evitar el desorden en el almacenamiento o simplemente para facilitar el trabajo del programador.

## ¿Cómo hacerlo?
Kotlin ofrece una forma muy sencilla de crear un archivo temporal utilizando la clase ```File``` y su función ```createTempFile()```. A continuación se muestra un ejemplo de cómo crear un archivo temporal en Kotlin:

```Kotlin
val temporaryFile = File.createTempFile("temp", ".txt")
println("El archivo temporal se ha creado en la ubicación: ${temporaryFile.absolutePath}")
```

La salida de este código será algo como "El archivo temporal se ha creado en la ubicación: /var/folders/7r/2w1d5c0x6k9chg7dpgcy8njm0000gn/T/temp8834372647141791213.txt". Como se puede ver, el nombre del archivo empieza con "temp" y termina con ".txt".

## Profundizando
La creación de archivos temporales ha sido una técnica utilizada por programadores desde hace mucho tiempo. Antes de la existencia de lenguajes de programación modernos, era común que los programadores crearan archivos temporales en sus sistemas para realizar tareas temporales.

En la actualidad, también existen otras opciones para realizar tareas similares como el uso de bases de datos en memoria o el uso de variables temporales en el código. Sin embargo, crear un archivo temporal sigue siendo una opción sencilla y efectiva para muchos escenarios.

Además, en términos de implementación, Kotlin utiliza la función ```createTempFile()``` de la clase ```File``` que internamente llama a las funciones del sistema operativo para crear el archivo temporal. Esto garantiza la eliminación automática del archivo y evita que queden archivos temporales innecesarios en el sistema.

## Ver también
Si quieres saber más sobre cómo trabajar con archivos en Kotlin, puedes consultar la documentación oficial en [kotlinlang.org](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/create-temp-file.html).

También puedes explorar otras opciones de almacenamiento temporal como el uso de la clase ```java.io.File.createTempFile``` en Java o la función ```System.IO.Path.GetTempFileName``` en C#.