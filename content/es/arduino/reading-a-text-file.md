---
title:    "Arduino: Leyendo un archivo de texto"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/arduino/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por qué

Leer archivos de texto es una habilidad muy útil para cualquier programador, ya que permite que nuestro Arduino interactúe con información almacenada en un archivo externo. Esto puede ser especialmente útil para proyectos que requieren una gran cantidad de datos o información cambiante.

## Cómo hacerlo

Para leer un archivo de texto en Arduino, primero debemos seguir los siguientes pasos:

1. Descargar e instalar la biblioteca "SD" en nuestro entorno de desarrollo de Arduino.
2. Conectar una tarjeta SD al Arduino (a través de un módulo SD o directamente a través de los pines).
3. En nuestro código, incluir la biblioteca "SD" y definir la variable para el objeto SD card.
4. Inicializar el objeto SD a través de la función "begin()".
5. Abrir el archivo de texto que queremos leer utilizando la función "open()".
6. Utilizar un bucle para leer el contenido del archivo y almacenarlo en una variable.
7. Cerrar el archivo utilizando la función "close()".

¡Y eso es todo! Ahora ya podemos manipular la información almacenada en nuestro archivo de texto. A continuación, se muestra un ejemplo de código que lee un archivo llamado "datos.txt" y lo muestra en el monitor serial:

```Arduino
#include <SD.h> //incluimos la biblioteca SD

File archivo; //variable para el archivo

void setup() {
  Serial.begin(9600); //inicializamos el monitor serial
  while (!Serial) {
    ; //esperamos a que el monitor serial se conecte  
  }

  if (!SD.begin(4)) { //inicializamos la tarjeta SD en el pin 4
    Serial.println("Error al inicializar la tarjeta SD!"); //si hay un error, lo mostramos en el monitor serial
    return;
  }

  archivo = SD.open("datos.txt"); //abrimos el archivo
    
  if (archivo) { //si el archivo se abrió correctamente
    while (archivo.available()) { //mientras haya datos disponibles en el archivo
      Serial.write(archivo.read()); //leemos y mostramos en el monitor serial
    }
    archivo.close(); //cerramos el archivo
  } else { //si no se pudo abrir el archivo
    Serial.println("Error al abrir archivo.txt!"); 
  }
}

void loop() {
  //nada aquí, ya que solo queremos leer el archivo una vez en el setup
}
```

El contenido del archivo "datos.txt" podría ser algo así:

```
Sensor de humedad: 50%
Temperatura: 25°C
Hora: 12:30
```

Y el resultado en el monitor serial sería:

```
Sensor de humedad: 50%
Temperatura: 25°C
Hora: 12:30
```

## Profundizando

Si queremos tomar un paso más allá en la lectura de archivos de texto en Arduino, podemos explorar otras funciones y métodos que nos permitirán tener un mejor control sobre la información leída. Por ejemplo, podemos utilizar la función "readBytes()" para leer una cierta cantidad de bytes del archivo en lugar de leer todo el contenido. También podemos utilizar las funciones "seek()" y "position()" para movernos a una posición específica dentro del archivo.

Investigar y experimentar con estas funciones nos dará una mejor comprensión de cómo manejar archivos de texto en Arduino.

¡Ahora ya sabes cómo leer archivos de texto en Arduino! Esto puede ser muy útil en una amplia variedad de proyectos y te permitirá tener una mayor flexibilidad y control sobre la información que manejas en tu placa.

## Ver también

1. Biblioteca SD de Arduino: https://www.arduino.cc/en/Reference/SD
2. Tutorial de SparkFun sobre lectura de archivos de texto en Arduino: https://learn.sparkfun.com/tutorials/sd-shield-and-qwiic-shield-hookup-guide/all#examples
3. Ejemplos de proyectos de Arduino que utilizan archivos de texto: https://randomnerdtutorials.com/arduino-microsd-card-module/