---
title:                "Kotlin: Leyendo un archivo de texto"
programming_language: "Kotlin"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por qué

Muchas veces, al trabajar con la programación, necesitamos leer y manipular archivos de texto. Ya sea para obtener información de un documento o para escribir en él, es una tarea muy común en el desarrollo de software. En este artículo te explicaremos cómo leer un archivo de texto en Kotlin y algunos detalles importantes a tener en cuenta.

## Cómo hacerlo

La forma más sencilla de leer un archivo de texto en Kotlin es utilizando la clase "File" de la biblioteca estándar de Java. A continuación, te mostramos un ejemplo de cómo hacerlo:

```Kotlin
val file = File("archivo.txt")

file.forEachLine {
    println(it)
}
```

En este ejemplo, primero declaramos una variable `file` de tipo `File` y le pasamos como argumento el nombre del archivo que queremos leer. Luego, utilizamos el método `forEachLine` para leer cada línea del archivo y mostrarla en la consola. Si quieres obtener todo el contenido del archivo como una sola cadena de texto, puedes utilizar el método `readText()` de la clase `File`:

```Kotlin
val file = File("archivo.txt")

val content = file.readText()

println(content)
```

También es importante tener en cuenta que al utilizar la clase `File`, debes manejar la excepción `FileNotFoundException` en caso de que el archivo no exista. Para ello, puedes utilizar un bloque `try-catch` o declarar que tu función lanza esa excepción.

## Profundizando

Hay muchas otras formas de leer un archivo de texto en Kotlin, incluyendo el uso de las clases `BufferedReader`, `Scanner` y `InputStream` de la biblioteca estándar de Java. Además, también puedes utilizar bibliotecas de terceros como Apache Commons IO o Kotlinx IO para leer archivos de forma más eficiente y con más opciones de manejo de excepciones.

Es importante tener en cuenta que al leer un archivo de texto, debes tener en cuenta la codificación utilizada en el archivo. Si no especificas una codificación, se utilizará la predeterminada del sistema, lo que puede causar problemas al leer caracteres especiales.

## Ver también

- [Documentación oficial de Kotlin sobre lectura de archivos](https://kotlinlang.org/docs/tutorials/kotlin-for-py/reading-files.html)
- [Tutorial sobre lectura de archivos en Kotlin](https://devexperto.com/lectura-escritura-archivos-kotlin/)
- [Ejemplo de lectura de archivos utilizando Kotlinx IO](https://www.baeldung.com/kotlin-read-file)