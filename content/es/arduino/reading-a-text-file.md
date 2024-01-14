---
title:                "Arduino: Leyendo un archivo de texto"
simple_title:         "Leyendo un archivo de texto"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/reading-a-text-file.md"
---

{{< edit_this_page >}}

# ¿Por qué deberías leer un archivo de texto en Arduino?

Si estás buscando una forma de almacenar y leer grandes cantidades de datos en tu proyecto de Arduino, leer un archivo de texto es una excelente opción. Puedes guardar cualquier tipo de información en un archivo de texto, como valores de sensores, mensajes o configuraciones. Además, leer un archivo de texto te permite modificar fácilmente los datos sin tener que reescribir tu código.

## Cómo leer un archivo de texto en Arduino

Para comenzar, necesitarás tener un archivo de texto en la misma ubicación que tu código de Arduino. Puedes crear y editar este archivo de texto en cualquier editor de texto en tu computadora. Asegúrate de guardarlo con la extensión ".txt".

Una vez que tengas tu archivo de texto, puedes usar la función `File` para abrirlo en tu código de Arduino. A continuación, puedes utilizar la función `readStringUntil()` para leer línea por línea el contenido del archivo.

Aquí hay un ejemplo de cómo leer un archivo de texto que contiene los valores de temperatura almacenados en líneas separadas:

```Arduino
// Abre el archivo de texto
File archivo = SD.open("temperaturas.txt");

// Lee una línea del archivo y la guarda como una cadena
String lectura = archivo.readStringUntil('\n');

// Imprime la línea leída en el monitor serial
Serial.println(lectura);

// Cierra el archivo
archivo.close();
```

La salida de este código mostrará la primera línea de tu archivo de texto, que en este caso sería el primer valor de temperatura guardado. Puedes continuar leyendo el archivo de esta manera para obtener todos los valores que necesitas.

## Profundizando en la lectura de archivos de texto

Existen muchas más funciones y métodos que puedes utilizar para leer archivos de texto en Arduino, como `parseInt()`para convertir los valores leídos en enteros o `parseFloat()` para convertirlos en números de punto flotante. También puedes usar bucles y condicionales para leer y procesar todo el contenido de un archivo de texto.

Además, es importante tener en cuenta que Arduino tiene un límite de memoria disponible para almacenar y procesar datos, por lo que asegúrate de no leer archivos de texto demasiado grandes o tu programa podría dejar de funcionar.

# Vea también

- Documentación oficial de Arduino sobre [lectura de archivos](https://www.arduino.cc/reference/en/libraries/sd/)
- Tutorial de SparkFun sobre [archivos de texto en Arduino](https://learn.sparkfun.com/tutorials/reading-and-writing-files-to-the-sd-card)
- Ejemplo de proyecto de [Arduino y SD card](https://www.instructables.com/id/Reading-and-Writing-Text-Files/)

¡Ahora estás listo para utilizar archivos de texto en tus proyectos de Arduino! Con esta función, podrás almacenar y leer grandes cantidades de datos de una manera fácil y organizada. ¡Experimenta con distintos tipos de archivos y encuentra la mejor forma de utilizarlos en tus proyectos!