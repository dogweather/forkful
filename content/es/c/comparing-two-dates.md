---
title:                "Comparando dos fechas"
html_title:           "C: Comparando dos fechas"
simple_title:         "Comparando dos fechas"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por qué

¿Alguna vez te has preguntado cómo se pueden comparar dos fechas en un programa? Comparar dos fechas puede ser útil en muchos casos, como por ejemplo en aplicaciones de gestión de eventos o de seguimiento de tareas. En este artículo, te mostraré cómo puedes comparar dos fechas en C de manera sencilla y eficiente.

## Cómo hacerlo

Para comparar dos fechas en C, primero debemos entender cómo se manejan las fechas en este lenguaje. En C, las fechas se representan con la estructura `tm`, que contiene información como el año, mes, día, entre otros.

Una forma de comparar dos fechas es convertir ambas en números enteros y luego compararlos. Por ejemplo, si tenemos dos fechas con la estructura `tm`, podemos utilizar la función `mktime` para convertirlas en tipo `time_t` y luego utilizar el operador comparador `>` o `<` para determinar cuál fecha es mayor o menor. Veamos un ejemplo:

```C
#include <stdio.h>
#include <time.h>

int main() {
  // Creamos dos estructuras con fechas diferentes
  struct tm fecha1 = { .tm_year = 2021, .tm_mon = 6, .tm_mday = 10 };
  struct tm fecha2 = { .tm_year = 2020, .tm_mon = 6, .tm_mday = 10 };

  // Convertimos ambas fechas a tipo time_t
  time_t t1 = mktime(&fecha1);
  time_t t2 = mktime(&fecha2);

  // Comparamos las fechas utilizando los operadores > y <
  if (t1 > t2) {
    printf("La fecha 1 es mayor que la fecha 2\n");
  } else if (t1 < t2) {
    printf("La fecha 2 es mayor que la fecha 1\n");
  } else {
    printf("Las fechas son iguales\n");
  }

  return 0;
}
```

El programa imprimirá "La fecha 1 es mayor que la fecha 2", ya que 2021 es mayor que 2020. Sin embargo, esta forma de comparar fechas puede ser un poco tediosa si tenemos que trabajar con muchas fechas diferentes. Para simplificar el proceso, podemos utilizar la función `difftime` que calcula la diferencia entre dos fechas en segundos, y luego dividirla entre la cantidad de segundos que hay en un día (86,400) para obtener el número de días de diferencia.

Veamos otro ejemplo, utilizando la función `difftime`:

```C
#include <stdio.h>
#include <time.h>

int main() {
  // Creamos dos estructuras con fechas diferentes
  struct tm fecha1 = { .tm_year = 2021, .tm_mon = 6, .tm_mday = 10 };
  struct tm fecha2 = { .tm_year = 2020, .tm_mon = 6, .tm_mday = 10 };

  // Convertimos ambas fechas a tipo time_t
  time_t t1 = mktime(&fecha1);
  time_t t2 = mktime(&fecha2);

  // Calculamos la diferencia entre ambas fechas en días
  double diff = difftime(t1, t2) / 86400;

  // Imprimimos el resultado
  printf("La diferencia entre las dos fechas es de %.0f días\n", diff);

  return 0;
}
```

El programa imprimirá "La diferencia entre las dos fechas es de 365 días", ya que hay un año de diferencia entre 2021 y 2020.

## Deep Dive

En este artículo hemos visto dos formas sencillas de comparar fechas en C. Sin embargo, hay muchas más funciones y métodos que se pueden utilizar para trabajar con fechas y tiempos en este lenguaje. Algunas de ellas son `localtime`, `difftime`, `mktime`, `gmtime` y `strftime`.

También es importante tener en cuenta que las fechas pueden variar dependiendo de la zona horaria en la que se encuentre el usuario. Por eso, es recomendable utilizar funciones específicas que se encarguen de manejar las diferencias horarias.

Para profundizar más en el tema de fechas y tiempos en C, te recomiendo revisar la documentación oficial y practicar con diferentes ejemplos.

## Ver también

- [Documentación oficial de C](https://devblogs.microsoft.com/oldnewthing/20030218-00/?p=42223)
- [