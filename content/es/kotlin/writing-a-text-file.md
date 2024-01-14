---
title:    "Kotlin: Escribiendo un archivo de texto"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Por qué escribir un archivo de texto

Escribir un archivo de texto es una habilidad esencial para cualquier programador, ya que permite almacenar y manipular datos de manera sencilla y eficiente. Además, los archivos de texto son un formato muy común y compatible con casi todos los lenguajes de programación, por lo que aprender a escribirlos te brindará una base sólida para trabajar en cualquier proyecto.

## Cómo escribir un archivo de texto en Kotlin

Para escribir un archivo de texto en Kotlin, primero debemos crear un objeto `File` que represente al archivo. Luego, utilizaremos el método `printWriter()` para crear un escritor que nos permitirá escribir en el archivo. A continuación, podemos utilizar el método `println()` para escribir líneas de texto en el archivo y finalmente, debemos cerrar el escritor para asegurarnos de que los datos se guarden correctamente. Veamos un ejemplo de código:

```Kotlin
//creamos el objeto File
val file = File("miArchivo.txt")

//creamos el escritor
val escritor = file.printWriter()

//escribimos en el archivo
escritor.println("Este es un ejemplo de texto.")
escritor.println("Podemos escribir varias líneas en un solo archivo.")

//cerramos el escritor
escritor.close()
```

Al ejecutar este código, se creará un archivo de texto llamado `miArchivo.txt` con las líneas de texto escritas en él. Si abrimos el archivo, veremos que cada línea está en una nueva línea, ya que utilizamos el método `println()` que agrega automáticamente un salto de línea al final de cada línea de texto.

## Profundizando en la escritura de archivos de texto

Además de escribir líneas de texto en un archivo, también es posible escribir otros tipos de datos, como números, arreglos y objetos. Para hacerlo, simplemente debemos convertir los datos en una cadena de texto utilizando el método `toString()` y luego escribirlos en el archivo como lo haríamos con una cadena. Por ejemplo:

```Kotlin
//escribimos un número en el archivo
val numero = 123
escritor.println(numero.toString())

//escribimos un arreglo en el archivo
val arreglo = arrayOf(1, 2, 3, 4, 5)
escritor.println(arreglo.toString())

//escribimos un objeto en el archivo
val objeto = Persona("Juan", 25)
escritor.println(objeto.toString())
```

Además, al utilizar el método `println()`, podemos utilizar la interpolación de cadenas de texto para escribir variables o expresiones directamente en el archivo. Por ejemplo:

```Kotlin
val nombre = "María"
escritor.println("Hola, mi nombre es $nombre.")
```

Este código escribirá en el archivo la línea "Hola, mi nombre es María." Esto es muy útil cuando queremos escribir datos dinámicos en un archivo.

## Ver también

- [Documentación oficial de Kotlin sobre escritura de archivos](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/print-writer.html)
- [Tutorial de programación en Kotlin para principiantes](https://kodepad.com/es/blog/aprendiendo-kotlin-programacion-basica-para-principiantes)