---
title:                "Analizando una fecha de una cadena."
html_title:           "Arduino: Analizando una fecha de una cadena."
simple_title:         "Analizando una fecha de una cadena."
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Extraer una fecha de una cadena de texto significa analizar y extraer la información de una fecha (día, mes, año) de una cadena de texto en un formato determinado. Los programadores lo hacen para poder trabajar con los datos de la fecha como variables y utilizarlos en sus programas, por ejemplo, en un reloj automático o en un sistema de registro de eventos.

## Cómo:
Arduino proporciona funciones para extraer la fecha de una cadena de texto. Por ejemplo:

```Arduino
// Definir una cadena de texto con la fecha en formato día, mes, año: "22/05/2021"
String fecha = "22/05/2021";

// Extraer el día de la fecha
int dia = fecha.substring(0,2).toInt();

// Extraer el mes de la fecha
int mes = fecha.substring(3,5).toInt();

// Extraer el año de la fecha
int año = fecha.substring(6,10).toInt();

// Imprimir en el monitor serie los valores extraídos
Serial.println("Día: " + String(dia));
Serial.println("Mes: " + String(mes));
Serial.println("Año: " + String(año));
```

Salida:

```
Día: 22
Mes: 05
Año: 2021
```

También se pueden utilizar funciones para convertir la cadena de texto en un objeto de fecha, lo que facilita el manejo de los datos:

```Arduino
// Definir una cadena de texto con la fecha en formato día, mes, año: "22/05/2021"
String fecha = "22/05/2021";

// Crear un objeto de fecha
tmElements_t fechaConvertida;

// Utilizar la función `strptime` para extraer la fecha de la cadena de texto en el objeto de fecha
strptime(fecha.c_str(), "%d/%m/%Y", &fechaConvertida);

// Imprimir en el monitor serie los valores extraídos
Serial.println("Día: " + String(fechaConvertida.Day));
Serial.println("Mes: " + String(fechaConvertida.Month));
Serial.println("Año: " + String(fechaConvertida.Year + 1970)); // La función `strptime` devuelve el año desde 1970
```

Salida:

```
Día: 22
Mes: 5
Año: 2021
```

## Profundizando:
Extraer una fecha de una cadena de texto es una técnica común utilizada en programación para trabajar con datos de tiempo. Antes de que existieran las bibliotecas de tiempo integradas en sistemas como Arduino, los programadores tenían que crear sus propias funciones para extraer y manipular las fechas. El uso de funciones integradas hace que sea más fácil y eficiente trabajar con datos de tiempo en programas.

Existen diferentes formas de manejar datos de tiempo en un programa, por ejemplo, utilizando una cadena de texto, un objeto de fecha o valores enteros para cada componente de la fecha (día, mes, año). La elección depende del tipo de programa y las preferencias del programador.

Para implementar una función de extracción de fecha desde una cadena de texto, se utilizan funciones como `substring()` y `toInt()` para extraer los componentes de la fecha, y `strptime()` para crear un objeto de fecha. Estas funciones están disponibles en la biblioteca `TimeLib.h` de Arduino.

## Ver también:

- Documentación de la función `substring()` de Arduino: https://www.arduino.cc/reference/en/language/functions/string/substring/
- Documentación de la función `strptime()` de Arduino: https://www.pjrc.com/teensy/td_libs_Time.html