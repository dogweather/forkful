---
title:                "Javascript: Calculando una fecha en el futuro o en el pasado."
programming_language: "Javascript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## ¿Por qué?

Calcular una fecha en el futuro o en el pasado puede ser útil para planificar eventos o realizar tareas programadas en una aplicación. Además, puede ser necesario en casos específicos donde se requiere conocer la fecha exacta en que ocurrió o sucederá algo.

## ¿Cómo?

Para calcular una fecha en el futuro o en el pasado en Javascript, se pueden utilizar los métodos `setDate()` y `getDate()` del objeto `Date`. Estos métodos permiten establecer y obtener el día del mes de una fecha determinada.

Por ejemplo, si queremos obtener la fecha de un evento que ocurrirá en 3 meses, podemos utilizar el siguiente código:

```Javascript
let fecha = new Date(); //fecha actual
fecha.setMonth(fecha.getMonth() + 3); //añadimos 3 meses
```

La nueva fecha será establecida en 3 meses a partir de la fecha actual.

También es posible calcular una fecha en el pasado, utilizando el método `setDate()` de la siguiente manera:

```Javascript
let fecha = new Date(); //fecha actual
fecha.setDate(fecha.getDate() - 10); //restamos 10 días
```

De esta forma, obtendremos la fecha que ocurrió hace 10 días.

## Profundizando

Para un cálculo más preciso, es importante tener en cuenta que no todos los meses tienen la misma cantidad de días. Por lo tanto, es necesario tomar en cuenta el año y el mes al realizar el cálculo.

Por ejemplo, si queremos obtener la fecha de dentro de 5 meses, pero queremos que el resultado sea en un año diferente, podemos utilizar el siguiente código:

```Javascript
let fecha = new Date(); //fecha actual
let año = fecha.getFullYear() + 1; //obtenemos el año actual y le sumamos 1
let mes = fecha.getMonth() + 5; //obtenemos el mes actual y le sumamos 5
fecha.setFullYear(año); //establecemos el nuevo año
fecha.setMonth(mes); //establecemos el nuevo mes
```

De esta forma, obtendremos la fecha exacta dentro de 5 meses en el próximo año.

## Ver también

- [Documentación de Date en Javascript](https://developer.mozilla.org/es/docs/Web/JavaScript/Referencia/Objetos_globales/Date)
- [Ejemplos de cálculos de fechas en Javascript](https://www.w3schools.com/js/js_dates.asp)
- [Más información sobre fechas en Javascript](https://www.digitalocean.com/community/tutorials/understanding-date-and-time-in-javascript)