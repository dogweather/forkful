---
title:                "Comprobando si existe un directorio"
html_title:           "Arduino: Comprobando si existe un directorio"
simple_title:         "Comprobando si existe un directorio"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por qué
¿Alguna vez te has preguntado cómo puedes comprobar si una carpeta existe en tu Arduino? Saber si una carpeta existe puede ser útil en muchas situaciones, por ejemplo, para asegurarte de que un archivo se guardó correctamente.

## Cómo hacerlo
Para comprobar si una carpeta existe, podemos utilizar la función `exists()` de la librería `SD` en Arduino. Primero, debemos inicializar la tarjeta SD con `SD.begin()` y luego podemos llamar a la función `exists()` con la ruta de la carpeta que queremos comprobar. Aquí hay un ejemplo de código:

```
Arduino SD Card = 10
char * carpeta = "/datos/";
if (SD.begin(10)) { 
     if (SD.exists(carpeta)) { 
         Serial.println("La carpeta existe."); 
     } else { 
         Serial.println("La carpeta no existe."); 
     } 
} 
```
El resultado de este código depende de si la carpeta "datos" existe en tu tarjeta SD. Si existe, verás el mensaje "La carpeta existe." en el Monitor Serie, de lo contrario, verás "La carpeta no existe."

## Profundizando
En este ejemplo, utilizamos la función `exists()` para comprobar la existencia de una carpeta, pero también podemos utilizarla para comprobar si un archivo existe. Solo debemos pasar la ruta del archivo en lugar de la carpeta.

Además, ten en cuenta que la función `exists()` solo comprueba la existencia de la carpeta o archivo en la ruta especificada, pero no comprueba si está vacío. Si quieres asegurarte de que la carpeta o archivo no esté vacío, puedes utilizar la función `open()` y luego verificar si se pudo abrir correctamente.

## Ver también
- [Referencia de la librería SD](https://www.arduino.cc/en/Reference/SD)
- [Tutorial sobre cómo utilizar una tarjeta SD con Arduino](https://randomnerdtutorials.com/complete-guide-for-ultrasonic-sensor-hc-sr04/)
- [Ejemplos de uso de la función `exists()`](https://forum.arduino.cc/t/sd-library-how-to-check-if-a-file-or-folder-exists/204581/6)