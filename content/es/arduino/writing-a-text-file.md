---
title:                "Arduino: Escribiendo un archivo de texto"
programming_language: "Arduino"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Por qué

Escribir un archivo de texto es una habilidad importante para programadores de Arduino ya que permite almacenar y recuperar información de manera eficiente. También es útil para guardar registros de datos o configuraciones para proyectos futuros.

## Cómo hacerlo

Primero, debemos incluir la biblioteca "SD.h" en nuestro sketch de Arduino. Luego, es importante establecer una conexión con la tarjeta SD utilizando el siguiente código:

```Arduino
if (!SD.begin(chipSelect)) {
  Serial.println("No se pudo iniciar la tarjeta SD");
  return;
}
```

Una vez que la conexión está establecida, podemos crear un objeto de archivo para nuestro archivo de texto utilizando la función "open". Luego, podemos escribir nuestro texto utilizando la función "println" y cerrar el archivo con la función "close". Aquí hay un ejemplo de código que escribe "Hello World" en un archivo llamado "datos.txt":

```Arduino
File datos = SD.open("datos.txt", FILE_WRITE);
if (datos) {
  datos.println("Hello World");
  datos.close();
}
```

Para ver si nuestro archivo se ha creado correctamente, podemos utilizar el programa de serie de Arduino para leer el archivo y ver su contenido.

## Profundizando

Cuando estamos escribiendo un archivo de texto en Arduino, podemos utilizar una variedad de funciones para dar formato a nuestro texto. Por ejemplo, podemos utilizar la función "print" para escribir texto sin un salto de línea al final, o la función "write" para escribir bytes individuales en lugar de cadenas de texto. También podemos utilizar la función "seek" para mover el puntero de archivo a una posición específica para sobrescribir o agregar texto en una ubicación determinada.

Es importante tener en cuenta que el tamaño de la tarjeta SD y la capacidad de almacenamiento de Arduino limitan la cantidad de datos que podemos escribir en un archivo de texto. Si necesitamos guardar grandes cantidades de datos, podemos considerar el uso de un módulo de memoria externa para ampliar la capacidad de almacenamiento.

## Ver también
- [Documentación de la biblioteca SD de Arduino](https://www.arduino.cc/en/Reference/SD)
- [Tutorial de escritura y lectura de archivos de texto en Arduino](https://www.c-sharpcorner.com/article/arduino-writing-and-reading-text-files/)
- [Métodos avanzados de formato y escritura de archivos de texto en Arduino](https://www.youtube.com/watch?v=AxHcU0Rr3ME)