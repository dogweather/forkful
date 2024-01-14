---
title:                "Arduino: Comparando dos fechas"
simple_title:         "Comparando dos fechas"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/comparing-two-dates.md"
---

{{< edit_this_page >}}

## ¿Por qué comparar dos fechas en Arduino?

Comparar dos fechas puede ser útil en varios proyectos de Arduino, como por ejemplo en un controlador de riego que necesita saber si ha pasado determinado tiempo desde la última vez que se regó una planta. También puede ser útil en proyectos que necesiten activar ciertas funciones en fechas específicas, como por ejemplo un calendario con recordatorios.

## Cómo hacerlo

Para comparar dos fechas en Arduino, necesitaremos utilizar las funciones proporcionadas por la librería "DateTime". Esta librería permite crear objetos de tipo "DateTime" que podemos utilizar para almacenar y comparar fechas. En el código a continuación, crearemos dos objetos de tipo "DateTime" y utilizaremos el operador "==" para comparar si son iguales. En este ejemplo, imprimiremos "Son iguales" si las fechas son iguales, y "Son diferentes" si no lo son.

```Arduino
#include <DateTime.h>

DateTime fecha1 = DateTime(2021, 10, 5);
DateTime fecha2 = DateTime(2021, 10, 5);

if (fecha1 == fecha2) {
  Serial.println("Son iguales");
} else {
  Serial.println("Son diferentes");
}

```

Si subimos este código al Arduino y abrimos el monitor serial, veremos que imprime "Son iguales", ya que ambas fechas son 5 de octubre de 2021.

## Profundizando en la comparación de fechas

Para poder comparar fechas de manera efectiva, es importante tener en cuenta que el formato de fecha utilizado por la librería "DateTime" es el siguiente: año/mes/día. Esto significa que si queremos comparar fechas anteriores al año 2000, debemos especificar el año con cuatro dígitos.

Además, también es importante tener en cuenta que la hora y los segundos no afectan a la comparación de fechas. Esto significa que si tenemos dos fechas con el mismo día, mes y año, pero con horas y/o segundos diferentes, el resultado de la comparación seguirá siendo "Son iguales".

## Ver también

- [Documentación de la librería "DateTime" en Arduino](https://www.arduino.cc/reference/es/libraries/datetime/)
- [Ejemplos de uso de la librería "DateTime" en Arduino](https://www.arduino.cc/en/Tutorial/DateTimeComparison)
- [Tutoriales sobre proyectos de Arduino con control de fechas](https://randomnerdtutorials.com/tag/date-and-time/)