---
title:                "Arduino: Obteniendo la fecha actual"
simple_title:         "Obteniendo la fecha actual"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por qué

Algunas veces, en nuestros proyectos de Arduino, necesitamos obtener la fecha actual. Puede ser para mostrarla en una pantalla LCD o para tomar decisiones basadas en la fecha. Aprender cómo obtener la fecha actual nos permite agregar funcionalidades interesantes a nuestros proyectos.

## Cómo hacerlo

Para obtener la fecha actual en Arduino, vamos a utilizar la librería "RTClib". Esta librería nos permite conectarnos a un módulo RTC (Real Time Clock) y obtener la fecha y hora en tiempo real.

Primero, necesitamos incluir la librería en nuestro código:

```
#include <RTClib.h>
```

Luego, declaramos un objeto de tipo "RTC_DS1307" que será el encargado de comunicarse con nuestro módulo RTC:

```
RTC_DS1307 rtc;
```

Antes de utilizar el objeto, debemos inicializarlo en el setup() de nuestro código:

```
void setup() {
  rtc.begin();
}
```

Ahora, podemos obtener la fecha actual usando el método "now()" del objeto rtc. Este método nos devuelve un objeto "DateTime" que podemos almacenar en una variable y utilizar sus métodos para obtener la información deseada:

```
DateTime now = rtc.now();
int dia = now.day();
int mes = now.month();
int año = now.year();
```

Podemos imprimir la fecha en un formato legible para el usuario utilizando el siguiente código:

```
Serial.print(dia);
Serial.print("/");
Serial.print(mes);
Serial.print("/");
Serial.println(año);
```

Con esto, ya hemos obtenido la fecha actual en nuestro código de Arduino.

## Profundizando

Si queremos aprender más sobre cómo funciona la obtención de la fecha actual en Arduino, podemos investigar sobre el formato de fecha utilizado por la librería RTClib y cómo se comunica con el módulo RTC a través del protocolo I2C.

También podemos explorar otras librerías que nos permitan obtener la fecha utilizando diferentes métodos, como conectarse a un servidor de tiempo a través de internet.

## Ver también

- [Librería RTClib](https://github.com/adafruit/RTClib)
- [Tutorial de Arduino sobre trabajar con fechas y tiempos](https://www.arduino.cc/en/Tutorial/libraryExamples/Time)
- [Información sobre el protocolo I2C](https://www.arduino.cc/en/reference/wire)