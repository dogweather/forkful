---
title:                "Comparando dos fechas"
html_title:           "Arduino: Comparando dos fechas"
simple_title:         "Comparando dos fechas"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/comparing-two-dates.md"
---

{{< edit_this_page >}}

## ¿Por qué comparar dos fechas?

Comparar fechas es una función importante en la programación, ya que nos permite realizar diferentes acciones basadas en la fecha actual o en fechas específicas. Por ejemplo, podemos utilizar la comparación de fechas para activar un sistema de riego solo en determinadas fechas o para mostrar un mensaje de felicitación en un día especial.

## Cómo hacerlo

La comparación de fechas en Arduino se basa en el uso de la librería "Time". Para poder utilizar esta librería, debemos incluirla en nuestro código en la sección de librerías:

```
#include <Time.h>
```

Una vez incluida la librería, podemos utilizar la función "time_t" para almacenar la fecha actual en una variable. Por ejemplo:

```
time_t fechaActual = now();
```

También podemos utilizar la función "Time" para definir una fecha específica y almacenarla en una variable. Por ejemplo:

```
tmElements_t fechaEspecifica;
fechaEspecifica.Year = 2020; //año
fechaEspecifica.Month = 12; //mes
fechaEspecifica.Day = 25; //día
fechaEspecifica.Hour = 0; //hora
fechaEspecifica.Minute = 0; //minuto
fechaEspecifica.Second = 0; //segundo
time_t fechaEspecificaSegundos = makeTime(fechaEspecifica);
```
En este ejemplo, hemos definido una fecha específica: 25 de diciembre de 2020 a las 0 horas. Utilizando la función "makeTime" podemos convertir esta fecha en segundos y almacenarla en la variable "fechaEspecificaSegundos".

Ahora que tenemos ambas fechas almacenadas en variables distintas, podemos utilizar operadores de comparación para determinar si una fecha es más reciente que la otra. Por ejemplo:

```
if(fechaEspecificaSegundos > fechaActual){
  //hacer algo si la fecha específica es más reciente
}
else if (fechaEspecificaSegundos < fechaActual){
  //hacer algo si la fecha específica es anterior o igual a la fecha actual
}
```

También podemos utilizar la función "timeStatus" para obtener el estado de la fecha y realizar acciones basadas en ese estado. Por ejemplo:

```
if(timeStatus() == timeNotSet){
  //hacer algo si la fecha no está configurada
}
else if (timeStatus() == timeSet){
  //hacer algo si la fecha está configurada
}
```

## Profundizando

La librería "Time" también ofrece funciones adicionales que nos permiten trabajar con fechas y horas de una manera más avanzada. Algunas de estas funciones son:

- `second()` : devuelve los segundos actuales.
- `minute()` : devuelve los minutos actuales.
- `hour()` : devuelve la hora actual en formato de 24 horas.
- `day()` : devuelve el día actual (del 1 al 31).
- `month()` : devuelve el mes actual (del 1 al 12).
- `year()` : devuelve el año actual desde 1970.
- `dayOfWeek()` : devuelve el día de la semana actual (del 1 al 7, donde 1 es domingo).
- `isLeapYear(year)` : devuelve un valor booleano que indica si el año es bisiesto o no.

Con estas funciones, podemos realizar comparaciones más precisas y realizar diferentes acciones basadas en la fecha y hora actual.

## Ver también

- Documentación oficial de la librería "Time": https://www.arduino.cc/reference/en/libraries/time/
- Tutoriales de comparación de fechas en Arduino: https://www.instructables.com/id/Arduino-Time-Library-Primer/ 
y https://how2electronics.com/arduino-hows/simple-date-time-example-arduino/
- Código de ejemplo: https://gist.github.com/as-ideas/0aeb932f8466f6a895d9e93460a39b21