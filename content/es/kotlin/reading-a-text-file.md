---
title:    "Kotlin: Leyendo un archivo de texto"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Por qué leer un archivo de texto en Kotlin

Muchas veces, en nuestros proyectos de programación, necesitamos leer datos almacenados en un archivo de texto. Ya sea para obtener una lista de nombres, una serie de números o cualquier otra información, leer un archivo de texto es una tarea común en el desarrollo de software. En este artículo, te explicaremos por qué es importante saber cómo leer un archivo de texto en Kotlin y cómo puedes hacerlo de manera efectiva.

## Cómo hacerlo

Para leer un archivo de texto en Kotlin, primero debes crear una variable que almacene la ruta del archivo que deseas leer. Luego, puedes utilizar la función `readText()` para leer el contenido del archivo y almacenarlo en una variable. Veamos un ejemplo de código:

```
var ruta = "ruta/archivo.txt"
var contenido = File(ruta).readText()
```

En este ejemplo, la variable `ruta` contiene la ruta del archivo que deseamos leer y la función `readText()` se encarga de leer el contenido del archivo y almacenarlo en la variable `contenido`.

## Detalle Técnico

Al leer un archivo de texto en Kotlin, es importante tener en cuenta la codificación del archivo. La codificación determina cómo se interpretan los caracteres del archivo y puede variar dependiendo del idioma y el sistema operativo en el que se creó el archivo. Por lo tanto, al leer un archivo de texto, es importante especificar la codificación que debe utilizarse. Por ejemplo:

```
var ruta = "ruta/archivo.txt"
var contenido = File(ruta, Charset.forName("UTF-8")).readText()
```

En este ejemplo, la función `forName()` indica que se utilizará la codificación UTF-8 para leer el archivo.

## Profundizando

Además de la función `readText()`, Kotlin también ofrece otras funciones útiles para leer archivos de texto, como `readLines()` y `forEachLine()`. Estas funciones te permiten leer el archivo de texto línea por línea y realizar acciones específicas en cada una. También puedes utilizar la clase `BufferedReader` para leer archivos de texto de manera más eficiente, especialmente en situaciones donde necesitas leer grandes archivos.

## Ver También

- [Documentación oficial de Kotlin sobre lectura de archivos](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/read-text.html)
- [Tutorial de lectura de archivos con Kotlin](https://www.baeldung.com/kotlin-read-file)