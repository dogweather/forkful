---
title:    "Arduino: Comprobando si existe un directorio"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Por qué
Hay muchas razones por las cuales un programador de Arduino puede querer validar si un directorio existe. Por ejemplo, puede ser parte de una función más grande que requiera la manipulación de archivos o puede ser necesario para una tarea específica en su proyecto.

## Cómo hacerlo
Es fácil comprobar si un directorio existe usando la librería SD incorporada en Arduino. Primero, necesitamos incluir la librería en nuestro código:

```Arduino
#include <SD.h>
```

Luego, definimos una variable para representar el directorio que queremos verificar. Por ejemplo, si queremos comprobar si el directorio "data" existe en la tarjeta SD, podemos hacer lo siguiente:

```Arduino
File dataDir = SD.open("data");
```

Ahora, podemos usar la función "exists" para verificar si el directorio existe o no. Esta función devolverá un valor booleano, true si el directorio existe y false si no existe.

```Arduino
if (dataDir.exists()) {
    Serial.println("El directorio existe");
} else {
    Serial.println("El directorio no existe");
}
```
El ejemplo completo quedaría así:

```Arduino
#include <SD.h>

void setup() {
    Serial.begin(9600);

    File dataDir = SD.open("data");

    if (dataDir.exists()) {
        Serial.println("El directorio existe");
    } else {
        Serial.println("El directorio no existe");
    }
}

void loop() {
    // código adicional
}
```

Si cargamos este código en nuestro Arduino y abrimos el monitor serial, veremos un mensaje indicando si el directorio existe o no. Recuerda que también puedes cambiar el nombre del directorio en la función "exists" para verificar otros directorios.

## Profundizando
Cuando usamos la función "exists" en la librería SD, en realidad estamos usando una función integrada en la clase "File". Esta clase nos permite interactuar con archivos y directorios en la tarjeta SD. La función "exists" en realidad es una abreviación de la función "existsdir", que comprueba si un directorio específico existe.

También es importante tener en cuenta que la función "exists" solo comprueba si el directorio está presente en la ubicación especificada, no verifica si hay archivos dentro de ese directorio. Para esto, se pueden utilizar otras funciones como "list" o "listFiles".

## Ver también
- [Documentación oficial de la librería SD para Arduino](https://www.arduino.cc/en/Reference/SD)
- [Tutorial sobre cómo manipular archivos en Arduino](https://randomnerdtutorials.com/arduino-sd-card-files/)

---

# Ver también