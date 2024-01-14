---
title:    "Arduino: Creando un archivo temporal"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/arduino/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Por qué

Hay muchas razones por las que uno podría necesitar crear un archivo temporal en su código de Arduino. Por ejemplo, si estás trabajando con datos que no quieres guardar permanentemente o si necesitas almacenar información temporalmente mientras realizas una tarea específica.

# Cómo hacerlo

Para crear un archivo temporal en Arduino, sigue estos sencillos pasos:
1. Primero, declara una variable de tipo `File` y utiliza su función `open` para crear el archivo temporal.
```
ArduinoFile tempFile = SD.open("temp.txt", FILE_WRITE);
```
2. Luego, puedes agregar datos al archivo temporal utilizando la función `println` de la variable `tempFile`.
```
tempFile.println("Datos temporales");
```
3. Finalmente, cierra el archivo temporal utilizando la función `close` de la variable `tempFile`.
```
tempFile.close();
```

Aquí hay un ejemplo completo de cómo crear y escribir datos en un archivo temporal en un SD card utilizando Arduino:
```
#include <SPI.h>
#include <SD.h>

File tempFile;

void setup() {
  Serial.begin(9600);
  
  // Inicializar la comunicación con la SD card
  if(!SD.begin(4)) {
    Serial.println("No se pudo iniciar la SD card.");
    while (1);
  }
  
  // Crear y abrir el archivo temporal en la SD card
  tempFile = SD.open("temp.txt", FILE_WRITE);
  
  // Escribir datos en el archivo temporal
  tempFile.println("Datos temporales");
  
  // Cerrar el archivo temporal
  tempFile.close();
}

void loop() {
  // Código adicional aquí
}
```

# Profundizando

Cuando creas un archivo temporal en Arduino, en realidad estás creando un archivo en la memoria de la SD card. Este tipo de memoria es conocida como memoria flash y almacena datos incluso cuando el dispositivo está apagado.

Al crear un archivo temporal, también puedes especificar el tipo de acceso que tendrá el archivo utilizando la función `open`. Por ejemplo, si cambias `FILE_WRITE` por `FILE_READ`, solo podrás leer el archivo temporal y no escribir en él.

También es importante destacar que los archivos temporales se deben eliminar una vez que ya no se necesitan. Puedes hacer esto utilizando la función `remove` de la clase `File`.

# Ver también

- [Tutorial de Arduino en español](https://www.arduino.cc/en/Tutorial/HomePage)
- [Documentación de SD.h](https://www.arduino.cc/en/Reference/SD)
- [Más información sobre memoria flash](https://www.electronicshub.org/introduction-to-flash-memory/)