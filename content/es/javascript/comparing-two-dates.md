---
title:                "Comparando dos fechas"
html_title:           "C#: Comparando dos fechas"
simple_title:         "Comparando dos fechas"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?
Comparar dos fechas es una tarea común en la programación, la cual determina si una fecha es mayor, menor o igual a otra. Esto es fundamental para funciones como perfilar el tiempo transcurrido, cuantificar deadlines y efectuar operaciones de contabilidad.

## Cómo:
```Javascript
let fecha1 = new Date(2020, 12, 31);
let fecha2 = new Date(2021, 1, 1);

if (fecha1.getTime() > fecha2.getTime()) {
    console.log("Fecha1 es después de Fecha2");
}
else if (fecha1.getTime() < fecha2.getTime()) {
    console.log("Fecha1 es antes de Fecha2");
}
else {
    console.log("Las fechas son iguales");
}
```
Salida de ejemplo:
```
“Fecha1 es antes de Fecha2”
```

## Inmersión Profunda
Históricamente, comparar fechas requirió operaciones manuales con descomposiciones de año, mes, día, hora, etc. Pero JavaScript ahora facilita esto con la función `Date`.

La alternativa al método `getTime()` es `valueOf()`, que devuelve los mismos resultados.

En las comparaciones entre fechas, JavaScript internamente convierte los objetos de fecha a milisegundos desde la fecha de inicio de UNIX (1º de enero de 1970), permite la comparación de manera precisa.

## Ver También
Para información más detallada:
- MDN Web Docs sobre [`Date`](https://developer.mozilla.org/es/docs/Web/JavaScript/Referencia/Objetos_globales/Date) 
- Referencias sobre [`getTime()`](https://developer.mozilla.org/es/docs/Web/JavaScript/Referencia/Objetos_globales/Date/getTime)y [`valueOf()`](https://developer.mozilla.org/es/docs/Web/JavaScript/Referencia/Objetos_globales/Date/valueOf)

Tutorial de JavaScript para principiantes en [w3schools](https://www.w3schools.com/js/).