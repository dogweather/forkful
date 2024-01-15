---
title:                "Creando un archivo temporal"
html_title:           "Java: Creando un archivo temporal"
simple_title:         "Creando un archivo temporal"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por qué

Crear un archivo temporal puede ser útil en diferentes situaciones, como por ejemplo cuando necesitamos almacenar datos temporales antes de guardarlos definitivamente, o cuando queremos realizar pruebas sin modificar nuestro archivo original. 

## Cómo hacerlo
Para crear un archivo temporal en Java, podemos utilizar la clase `File` y el método `createTempFile()`, pasando como parámetro el prefijo y sufijo que queramos para el nombre del archivo. Aquí te dejamos un ejemplo de código:

```Java
File tempFile = File.createTempFile("tempFile", ".txt"); 
```

Este código creará un archivo temporal en la ubicación por defecto del sistema operativo y con el nombre "tempFileXXXXXX.txt", donde las X son números aleatorios generados automáticamente. 

Para escribir en el archivo, podemos utilizar la clase `FileWriter` y el método `write()`, de la siguiente manera:

```Java
String data = "Este es un ejemplo de texto que se escribirá en el archivo temporal.";
FileWriter writer = new FileWriter(tempFile);
writer.write(data);
```

Al ejecutar este código, se escribirá el texto en el archivo temporal creado. 

A continuación, podemos leer el contenido del archivo utilizando la clase `BufferedReader` y el método `readLine()`, según se muestra en el siguiente código:

```Java
BufferedReader reader = new BufferedReader(new FileReader(tempFile));
String line;
while ((line = reader.readLine()) != null) {
    System.out.println(line); //Imprime el contenido del archivo por consola
}
```

El resultado de la ejecución de este código será la impresión del texto contenido en el archivo temporal por consola.

## Profundizando
Al crear un archivo temporal en Java, también podemos especificar una ubicación diferente a la por defecto mediante el método `createTempFile()` pasando como parámetro un `File` para la ubicación y, opcionalmente, la codificación del archivo.

También podemos indicar al sistema operativo que elimine el archivo temporal al finalizar el programa, utilizando el método `deleteOnExit()` después de escribir en el archivo.

Finalmente, es importante asegurarse de cerrar todos los recursos utilizados al finalizar el programa: el archivo, el escritor y el lector. Para eso, se pueden utilizar los métodos `close()` de cada clase correspondiente.

## Véase también
- [Documentación oficial de Java sobre la clase `File`](https://docs.oracle.com/javase/8/docs/api/java/io/File.html)
- [Tutorial de Java sobre creación y manipulación de archivos](https://www.tutorialspoint.com/java/java_files_io.htm)
- [Ejemplo de código de creación de archivos temporales en Java](https://www.geeksforgeeks.org/filecreatefile-java/)

¡Ahora ya puedes crear y utilizar archivos temporales en tus programas Java! Esto puede ser muy útil para manejar datos temporales sin afectar tus archivos originales y para realizar pruebas en un entorno seguro. ¡No dudes en probarlo y experimentar con otras funcionalidades!