---
title:                "Leyendo un archivo de texto"
html_title:           "Arduino: Leyendo un archivo de texto"
simple_title:         "Leyendo un archivo de texto"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/reading-a-text-file.md"
---

{{< edit_this_page >}}

# ¿Por qué leer un archivo de texto en Arduino?

Si estás familiarizado con Arduino, sabrás que es una herramienta versátil que se utiliza para controlar diferentes componentes y realizar diversas tareas. Una de esas tareas puede ser leer un archivo de texto, lo que puede ser útil para almacenar y recuperar datos o configuraciones en tu proyecto de Arduino.

# Cómo hacerlo

Para leer un archivo de texto en Arduino, necesitarás los siguientes componentes:
- Una placa de Arduino (cualquier versión debería funcionar)
- Un lector de tarjetas SD
- Una tarjeta SD
- Cables de conexión
- Un archivo de texto con los datos que deseas leer (asegúrate de que esté guardado en la tarjeta SD)

Antes de comenzar con el código, es importante mencionar que el lector de tarjetas SD debe estar conectado a los pines SPI de tu placa de Arduino. Ahora, veamos el código necesario para leer un archivo de texto en Arduino:

```
#include <SPI.h>  // Incluimos la librería para manejar el lector de tarjetas SD
#include <SD.h>   // Incluimos la librería para leer tarjetas SD

File myFile;  // Creamos una variable para almacenar nuestro archivo de texto

void setup() {
   Serial.begin(9600);  // Iniciamos la comunicación serial a una velocidad de 9600 baudios
   while (!Serial) {
      ;  // Esperamos a que esté lista la comunicación serial (sólo necesario para algunas placas de Arduino)
   }

   Serial.print("Iniciando lector de tarjetas SD...");
   if (!SD.begin(4)) {  // Iniciamos el lector de tarjetas SD en el pin 4 (puedes cambiarlo si lo necesitas)
      Serial.println("¡Fallo!");
      while (1);  // En caso de error, detenemos la ejecución del programa
   }
   Serial.println("¡Listo!");
   
   myFile = SD.open("archivo.txt");  // Abrimos nuestro archivo de texto y lo almacenamos en la variable `myFile`
   
   if (myFile) {  // Si el archivo existe
      Serial.println("Leyendo archivo...");
      
      // Leemos línea por línea y lo imprimimos en la consola serial
      while (myFile.available()) {
         Serial.write(myFile.read());
      }
      myFile.close();  // Cerramos el archivo
   } else {
      // Si el archivo no existe, imprimimos un mensaje de error
      Serial.println("¡Error al abrir el archivo!");
   }
}

void loop() {
   // El código para leer y mostrar el archivo en la consola serial sólo se ejecuta una vez en el setup
}
```

La salida en la consola serial debería ser algo como esto:
```
Iniciando lector de tarjetas SD...¡Listo!
Leyendo archivo...
Contenido de archivo de texto.
¡Todo listo!
```

# Profundizando en la lectura de archivos de texto

Mientras que este código básico es suficiente para leer un archivo de texto en Arduino, existen otras funciones y métodos que pueden ser útiles dependiendo de tus necesidades. Por ejemplo, si quieres leer sólo una parte específica del archivo o si necesitas almacenar los datos en variables para utilizarlos en tu código.

Una función importante es `read()`, que permite leer carácter por carácter en lugar de línea por línea. También puedes utilizar `seek()` para moverte a una posición específica en el archivo y `readBytes()` si quieres leer un número determinado de bytes en lugar de una línea o carácter específico.

Si deseas profundizar aún más, la documentación oficial de Arduino tiene una lista completa de funciones y métodos disponibles para la lectura de archivos de texto.

# Ver también

- [Documentación oficial de Arduino sobre lectura de archivos de texto](https://www.arduino.cc/en/Reference/SD)
- [Ejemplo de lectura de un archivo de texto en Arduino](https://www.circuitbasics.com/arduino-basics-read-a-text-file-and-print-it/)
- [Ejemplos de proyectos con lectura de archivos de texto en Arduino](https://create.arduino.cc/projecthub/search?q=text%20file&category=other&input=)