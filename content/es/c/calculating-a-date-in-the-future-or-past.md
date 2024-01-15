---
title:                "Calculando una fecha en el futuro o pasado"
html_title:           "C: Calculando una fecha en el futuro o pasado"
simple_title:         "Calculando una fecha en el futuro o pasado"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Por qué
Calculando fechas en el futuro o pasado es una habilidad útil en la programación ya que permite a los desarrolladores crear aplicaciones que manejen eventos y tareas futuras, como recordatorios o reservaciones. También puede ser útil cuando se trabaja con marcos de tiempo específicos en aplicaciones de finanzas o logística.

## Cómo hacerlo
Para calcular una fecha en el futuro o pasado en C, primero necesitamos obtener la fecha actual. Esto se puede lograr utilizando la función `time()` de la biblioteca `time.h`. Luego, podemos usar la función `localtime()` para convertir el tiempo actual en una estructura de fecha y hora.

```C
// Obtener la fecha actual
time_t ahora;
time(&ahora);

// Convertir a estructura de fecha y hora
struct tm *fecha = localtime(&ahora);

// Obtener día, mes y año actual
int dia = fecha->tm_mday;
int mes = fecha->tm_mon + 1;
int anio = fecha->tm_year + 1900;
```

Una vez que tenemos la fecha actual, podemos usar funciones como `mktime()` o `strptime()` para manipular la fecha y obtener una nueva fecha en el futuro o pasado. También podemos usar operadores matemáticos para sumar o restar días, meses o años a la fecha actual.

Por ejemplo, si queremos calcular la fecha 10 días en el futuro, podemos hacer lo siguiente:

```C
// Sumar 10 días
fecha->tm_mday += 10;

// Convertir nuevamente a tiempo en segundos
time_t fecha_futura = mktime(fecha);

// Imprimir la nueva fecha en formato dd/mm/yyyy
printf("La fecha 10 días en el futuro es: %02d/%02d/%d", fecha->tm_mday, fecha->tm_mon + 1, fecha->tm_year + 1900);
```

El resultado de este código sería `La fecha 10 días en el futuro es: 18/05/2021`.

## Deep Dive
Para calcular una fecha en el futuro o pasado, es importante entender cómo funciona el sistema de tiempo en C. Las fechas en C se almacenan en una estructura de fecha y hora, que consta de variables para el día, mes, año, hora, minutos, segundos, entre otras.

Algunas funciones importantes para manipular fechas en C son `mktime()`, que convierte una fecha en una cadena de tiempo (en segundos), y `strftime()`, que convierte una cadena de tiempo en una fecha legible. También hay funciones específicas para sumar o restar días, meses o años a una fecha determinada, como `add_days()` o `add_years()`.

Otra cosa importante a tener en cuenta es que la fecha y hora dependen del sistema en el que se esté ejecutando el programa. Por lo tanto, es posible que los resultados no sean consistentes en diferentes sistemas operativos o configuraciones de zona horaria.

## Ver también
- [Documentación de C en línea](https://www.cplusplus.com/reference/clibrary/)
- [Manipulación de fechas en C - Tutorial de Guru99](https://www.guru99.com/c-date-time.html)
- [Funciones de manejo de fecha y hora en C - GeeksforGeeks](https://www.geeksforgeeks.org/date-time-functions-in-c-c-with-examples/)