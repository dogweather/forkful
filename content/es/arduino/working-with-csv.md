---
title:                "Trabajando con csv"
html_title:           "Arduino: Trabajando con csv"
simple_title:         "Trabajando con csv"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/working-with-csv.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
CSV son las siglas de "Valores Separados por Comas", y se refiere a un tipo de formato de archivo que se utiliza para almacenar datos en una tabla con valores separados por comas. Los programadores utilizan este formato porque es fácil de leer y escribir para los humanos, y también puede ser fácilmente procesado por los programas.

## Cómo hacerlo:
Para trabajar con CSV en Arduino, necesitarás importar la biblioteca "ArduinoCSV" en tu código. Luego, puedes utilizar la función "createStream" para crear un archivo CSV y escribir datos en él. Por ejemplo:

```Arduino
#include <ArduinoCSV.h>

File sampleCSV;

void setup() {
  Serial.begin(9600);
  sampleCSV = createStream("datos.csv", 6);
  sampleCSV.println("Sensor 1, Sensor 2, Sensor 3, Sensor 4, Sensor 5, Sensor 6");
  sampleCSV.close();
}

void loop() {
  int sensor1 = analogRead(A0);
  int sensor2 = analogRead(A1);
  int sensor3 = analogRead(A2);
  int sensor4 = analogRead(A3);
  int sensor5 = analogRead(A4);
  int sensor6 = analogRead(A5);
  
  sampleCSV = createStream("datos.csv", 6);
  sampleCSV.print(sensor1);
  sampleCSV.print(",");
  sampleCSV.print(sensor2);
  sampleCSV.print(",");
  sampleCSV.print(sensor3);
  sampleCSV.print(",");
  sampleCSV.print(sensor4);
  sampleCSV.print(",");
  sampleCSV.print(sensor5);
  sampleCSV.print(",");
  sampleCSV.print(sensor6);
  sampleCSV.close();
  
  delay(1000);
}
```
Este código creará un archivo CSV llamado "datos.csv" y escribirá los valores de seis sensores en él, separados por comas. Luego, en el bucle loop, se actualizarán los valores y se escribirán en el archivo CSV cada segundo.

El resultado en el archivo CSV se verá así:

```
Sensor 1, Sensor 2, Sensor 3, Sensor 4, Sensor 5, Sensor 6
145, 223, 301, 402, 502, 621
146, 220, 298, 401, 498, 627
149, 222, 299, 400, 500, 622
```

## Profundizando
CSV se ha utilizado ampliamente desde la década de 1980, especialmente en aplicaciones de hojas de cálculo. Sin embargo, hay otros formatos de archivo que también se utilizan para almacenar datos tabulares, como JSON y XML.

La biblioteca "ArduinoCSV" viene con una función extra llamada "setDataDelimiter", que te permite cambiar el delimitador de los valores en lugar de una coma. Por ejemplo, puedes utilizar un punto y coma o un tabulador en su lugar.

## Ver también:
- Documentación de la biblioteca ArduinoCSV: https://github.com/arduino-libraries/ArduinoCSV
- Documentación oficial de Arduino sobre el manejo de archivos: https://www.arduino.cc/en/Tutorial/FileIO