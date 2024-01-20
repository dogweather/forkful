---
title:                "Analizando una fecha a partir de una cadena de texto"
html_title:           "Bash: Analizando una fecha a partir de una cadena de texto"
simple_title:         "Analizando una fecha a partir de una cadena de texto"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y Por qué?

La interpretación de una fecha desde una cadena significa extraer la información de día, mes y año a partir de una cadena de texto. Esto es útil para manejar fechas entrantes de diversas fuentes, como sensores o redes, en un formato entendible para Arduino.

## ¿Cómo hacerlo?

Aquí hay un ejemplo de cómo interpretar una fecha a partir de una cadena en Arduino:

```Arduino 
#include <Time.h>    //Incluye la librería Time
#include <TimeLib.h> //Incluye la librería TimeLib

String date = "25/12/2022"; //Definimos la cadena 

int day = date.substring(0,2).toInt(); //Día
int month = date.substring(3,5).toInt(); //Mes
int year = date.substring(6,10).toInt(); //Año

tmElements_t tm;

tm.Day = day;
tm.Month = month;
tm.Year = CalendarYrToTm(year);

time_t t = makeTime(tm); //Convierte la fecha a formato de tiempo
```

El resultado será una fecha convertida a un formato que Arduino puede entender y manipular.

## Profundizando

Historia: La extracción de información de fechas desde cadenas de texto ha sido una práctica común desde los primeros días de la programación, debido a las diversas formas en las que se puede representar una fecha.

Alternativas: Existen otras formas de interpretar una fecha, dependiendo del formato original de la cadena. Podríamos usar expresiones regulares o una serie de funciones `substring()` si el formato varía.

Detalles de Implementación: Este método hace uso de las funciones `substring()`, `toInt()`, y `makeTime()`, que extraen subcadenas de la cadena original, convierten esas subcadenas a enteros y crean un objeto de tiempo, respectivamente.

## Para saber más

Puede encontrar más detalles en la documentación oficial de Arduino:

[Documentación de la librería Time](https://www.arduino.cc/reference/en/libraries/time/)

[Funciones de la Clase String](https://www.arduino.cc/reference/es/language/variables/data-types/string/functions/)

[Conversión de tiempo y fecha en Arduino](https://playground.arduino.cc/Code/time)