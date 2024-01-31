---
title:                "Trabajando con archivos CSV"
date:                  2024-01-19
simple_title:         "Trabajando con archivos CSV"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/working-with-csv.md"
---

{{< edit_this_page >}}

## Qué es y Por Qué?
Trabajar con CSV (valores separados por comas) significa manejar datos de manera simple y estructurada. Los programadores usan CSV para importar y exportar datos de sensores, configuraciones o cualquier información que necesite ser fácilmente leída por humanos y máquinas.

## Cómo:
Primero, crea un archivo CSV en la tarjeta SD conectada al Arduino. Asegúrate de tener la biblioteca `SD.h` incluida.

```Arduino
#include <SD.h>

File myFile;

void setup() {
  Serial.begin(9600);
  SD.begin(10); // Asume que el pin CS de tu SD está en el pin 10
  myFile = SD.open("datos.csv", FILE_WRITE);

  // Escribir encabezados de columna:
  myFile.println("Tiempo, Temperatura, Humedad");
  
  // Agregar datos:
  myFile.println("0, 23.5, 60");
  myFile.println("1, 24.0, 58");
  myFile.println("2, 24.3, 57");

  myFile.close();
}

void loop() {
  // Aquí podrías seguir agregando datos regularmente
}
```

Y para leer datos:

```Arduino
void setup() {
  Serial.begin(9600);
  SD.begin(10);
  myFile = SD.open("datos.csv");
  
  if (myFile) {
    while (myFile.available()) {
      Serial.write(myFile.read());
    }
    myFile.close();
  } else {
    Serial.println("Error al abrir el archivo");
  }
}

void loop() {
  // Leer datos podría ser parte de una acción periódica o bajo demanda
}
```

Salida esperada (será visualizada en el monitor serial):
```
Tiempo, Temperatura, Humedad
0, 23.5, 60
1, 24.0, 58
2, 24.3, 57
```

## Profundización
El formato CSV existe desde los primeros días de las computadoras personales, facilitando el intercambio de datos entre programas diferentes y sistemas operativos. Existen alternativas como JSON o XML, pero CSV sigue siendo popular debido a su simplicidad. En Arduino, manejar CSV es una forma efectiva de almacenar lecturas de datos debido a la memoria y capacidad de procesamiento limitados.

## Ver También
- Documentación de la librería SD de Arduino: https://www.arduino.cc/en/reference/SD
- Tutorial sobre cómo usar una tarjeta SD con Arduino: https://www.arduino.cc/en/tutorial/readWriteSD
- Información general sobre el formato CSV: https://es.wikipedia.org/wiki/Valores_separados_por_comas
