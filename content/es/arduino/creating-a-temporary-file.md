---
title:                "Arduino: Creando un archivo temporal"
simple_title:         "Creando un archivo temporal"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por qué

Crear un archivo temporal puede ser útil en muchas situaciones, ya sea para almacenar datos temporales o para realizar pruebas en nuestro programa sin afectar archivos permanentes. Además, puede ayudarnos a ahorrar espacio de almacenamiento en nuestra placa Arduino.

## Cómo hacerlo

Para crear un archivo temporal en Arduino, podemos seguir los siguientes pasos:

1. Incluir la librería SD en nuestro código.

```Arduino
#include <SD.h>
```

2. Definir una variable para almacenar el nombre de nuestro archivo temporal.

```Arduino
String tempFile = "temp.txt";
```

3. Abrir el archivo temporal en modo escritura.

```Arduino
File file = SD.open(tempFile, FILE_WRITE);
```

4. Escribir los datos que deseamos almacenar en el archivo.

```Arduino
file.print("Datos temporales");
```

5. Cerrar el archivo.

```Arduino
file.close();
```

Con estos sencillos pasos, habremos creado un archivo temporal en nuestra tarjeta SD. Podemos verificarlo conectando la tarjeta SD a nuestro ordenador y buscando el archivo "temp.txt".

## Profundizando

Al crear un archivo temporal, también debemos tener en cuenta que este archivo ocupará espacio en nuestra tarjeta SD. Por lo tanto, es importante borrarlo después de su uso. Podemos hacerlo utilizando la función "remove()" de la librería SD.

```Arduino
SD.remove(tempFile);
```

Además, es recomendable utilizar un nombre de archivo único para evitar conflictos con otros archivos. Podemos hacerlo agregando algún dato diferente o aleatorio al nombre del archivo, como la hora o un número generado por la placa.

## Ver también

- [Tutorial de la librería SD en Arduino](https://www.arduino.cc/en/Reference/SD)
- [Más información sobre archivos temporales](https://en.wikipedia.org/wiki/Temporary_file)