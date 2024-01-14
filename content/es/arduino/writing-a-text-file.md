---
title:    "Arduino: Escribiendo un archivo de texto"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/arduino/writing-a-text-file.md"
---

{{< edit_this_page >}}

## ¿Por qué escribir un archivo de texto?

Escribir un archivo de texto puede ser útil para almacenar datos o información que necesitamos en nuestro proyecto de Arduino. También puede ser una forma de guardar registros o resultados de nuestro código.

## Cómo hacerlo

Para escribir un archivo de texto en Arduino, necesitamos seguir los siguientes pasos:

- Primero, abrimos el archivo que queremos escribir. Puedes hacerlo utilizando la función `File.open()`, especificando el nombre del archivo y "w" como parámetros para indicar que queremos escribir en el archivo.
- Luego, utilizamos la función `File.print()` o `File.println()` para escribir en el archivo. `Print()` escribirá los datos en una sola línea, mientras que `println()` añadirá un salto de línea al final.
- Una vez que hayamos terminado de escribir en el archivo, lo cerramos utilizando la función `File.close()`.

Aquí hay un ejemplo de cómo escribir un texto simple en un archivo llamado "datos.txt":

```Arduino
File archivo = SD.open("datos.txt", "w");             // Abrimos el archivo para escribir
archivo.println("Este es un ejemplo de texto.");      // Escribimos en el archivo
archivo.close();                                     // Cerramos el archivo

```

## Profundizando

Si queremos escribir datos más complejos en nuestro archivo de texto, podemos utilizar las funciones `File.write()` o `File.writeBytes()`. Estas funciones nos permiten escribir datos en formato binario, lo que puede ser útil para almacenar más información en un solo archivo.

También es importante tener en cuenta que, al trabajar con Arduino, tenemos un espacio limitado para almacenar archivos. Por lo tanto, debemos asegurarnos de no escribir demasiados datos en un solo archivo o podemos quedarnos sin memoria.

## Vea también

- [Cómo leer un archivo de texto en Arduino](https://www.arduino.cc/en/Tutorial/ReadASCIIString)
- [Documentación oficial de la clase File de Arduino](https://www.arduino.cc/en/Reference/SDFile)
- [Ejemplo de escritura de archivo con Arduino Mega](https://create.arduino.cc/projecthub/techmirtz/writing-and-reading-files-on-sd-card-with-arduino-mega-2560-eff893)