---
title:                "Calculando una fecha en el futuro o en el pasado"
html_title:           "Arduino: Calculando una fecha en el futuro o en el pasado"
simple_title:         "Calculando una fecha en el futuro o en el pasado"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# ¿Por qué necesitamos calcular fechas en el futuro o pasado?

A veces, es necesario saber qué fecha será en una determinada cantidad de días en el futuro o en el pasado. Esto puede ser útil para planificar eventos o tareas, o simplemente por curiosidad. Con Arduino, podemos crear un programa sencillo que nos calculará automáticamente la fecha deseada.

## Cómo hacerlo

Para calcular una fecha en el futuro o en el pasado, necesitaremos tres variables: día, mes y año. En nuestro código, utilizaremos la función `day()` para obtener el día actual, `month()` para obtener el mes actual y `year()` para obtener el año actual. También necesitaremos una variable para almacenar la cantidad de días que queremos sumar o restar.

```Arduino
int diaActual = day();  // día actual
int mesActual = month();  // mes actual
int añoActual = year();  // año actual
int diasExtra = 30;  // cantidad de días para sumar o restar

// Calculamos la fecha sumando los días extra a la fecha actual
int diaFuturo = diaActual + diasExtra;  
int mesFuturo = mesActual;
int añoFuturo = añoActual;

// Si el día futuro es mayor al número de días del mes, ajustamos el mes y el día
if (diaFuturo > 31) {
  diaFuturo -= 31;
  mesFuturo++;  
}

// Si el mes futuro es mayor a 12, ajustamos el mes y el año
if (mesFuturo > 12) {
  mesFuturo -= 12;
  añoFuturo++;  
}

// Imprimimos la fecha en el monitor serie
Serial.print("La fecha en ");
Serial.print(diasExtra);
Serial.print(" días será: ");
Serial.print(diaFuturo);
Serial.print("/");
Serial.print(mesFuturo);
Serial.print("/");
Serial.println(añoFuturo);

```

Ahora el código está listo para calcular y mostrar la fecha en la cantidad de días especificada. Puedes modificar la variable `diasExtra` para calcular diferentes fechas en el futuro o en el pasado.

## Profundizando más

Si quieres calcular fechas más precisas, puedes incluir otras variables en tu código, como el número de días en cada mes o años bisiestos. También puedes agregar una función para validar si la fecha que quieres mostrar es válida o no. Además, puedes practicar con otras operaciones, como calcular la diferencia entre dos fechas.

# Ver también

- [Documentación oficial de Arduino](https://www.arduino.cc/reference/es/)
- [Cálculo de la fecha juliana en Wikipedia](https://es.wikipedia.org/wiki/Fecha_juliana)