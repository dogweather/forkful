---
title:    "Arduino: Leyendo un archivo de texto."
keywords: ["Arduino"]
---

{{< edit_this_page >}}

# Por qué

Aprender a leer archivos de texto en Arduino puede ser útil para proyectos que requieren almacenamiento de datos o la comunicación con otros dispositivos. Con esta habilidad, podrás crear proyectos más complejos y versátiles.

## Cómo hacerlo

Existen varias maneras de leer un archivo de texto en Arduino. Una forma sencilla es utilizando la función `SD.open()` para abrir el archivo y luego utilizar la función `read()` para obtener los datos del archivo. Puedes encontrar un ejemplo de código a continuación:

```
Arduino~~~cpp

#include <SD.h> // Incluir biblioteca para manejar la tarjeta SD

File archivo; // Crear un objeto archivo

void setup() {
  Serial.begin(9600); // Iniciar la comunicación serial
  SD.begin(10); // Iniciar la tarjeta SD en el pin 10
  archivo = SD.open("datos.txt"); // Abrir el archivo "datos.txt"
}

void loop() {
  if (archivo) { // Si el archivo se abre correctamente
    while (archivo.available()) { // Mientras haya datos en el archivo
      Serial.write(archivo.read()); // Mostrar los datos en la comunicación serial
    }
    archivo.close(); // Cerrar el archivo
  } else {
    Serial.println("Error al abrir el archivo."); // Mostrar mensaje de error en caso de que el archivo no se abra correctamente
  }
}

```

Puedes modificar este código según tus necesidades y el formato de tu archivo de texto.

## Inmersión profunda

Además de utilizar la función `read()`, también puedes utilizar otras funciones para leer datos específicos del archivo, como `readString()` para leer una cadena de texto y `readInt()` para leer un entero. También es importante tener en cuenta el formato de tu archivo de texto, ya que puede afectar la forma en que se leen los datos.

Si deseas obtener más información sobre la lectura de archivos de texto en Arduino, puedes consultar la documentación oficial de la biblioteca SD en el sitio web de Arduino o buscar tutoriales en línea.

## Ver también

- Tutorial de Arduino sobre cómo leer archivos de texto: https://www.arduino.cc/en/Tutorial/ReadASCIIString
- Ejemplo de lectura de un archivo de texto en Arduino: https://www.instructables.com/Arduino-Reading-Text-Files/
- Documentación oficial de la biblioteca SD: https://www.arduino.cc/en/Reference/SD