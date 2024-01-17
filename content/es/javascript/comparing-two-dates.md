---
title:                "Comparando dos fechas"
html_title:           "Javascript: Comparando dos fechas"
simple_title:         "Comparando dos fechas"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## ¿Qué y Por qué?

Comparar dos fechas en Javascript es una acción recurrente en la programación, especialmente cuando se trabaja con fechas y horarios. Se trata de comparar dos objetos de tipo ```Date```, ya sea para determinar si son iguales, anteriores o posteriores entre sí.

Los programadores realizan esta tarea para poder ordenar listas de fechas, validar la entrada de datos en formularios, calcular la diferencia entre dos fechas y muchas otras tareas relacionadas con el manejo de fechas y tiempos.

## ¿Cómo hacerlo?

A continuación se muestran algunos ejemplos de cómo comparar dos fechas en Javascript usando operadores lógicos y métodos específicos de objetos ```Date```. Se incluye también un ejemplo de salida para cada caso.

```Javascript
let fecha1 = new Date('2021-01-31');
let fecha2 = new Date('2021-02-28');

fecha1 < fecha2; // true
fecha1 === fecha2; // false

fecha1.getTime() < fecha2.getTime(); // true
fecha1.getTime() === fecha2.getTime(); // false

fecha1.toDateString() === fecha2.toDateString(); // false
fecha2.toDateString() > fecha1.toDateString(); // true
```

Salida:
```
true
false
true
false
false
true
```

## Profundizando

Históricamente, la comparación de fechas ha sido un desafío en la programación debido a la diversidad de formatos y sistemas de representación de fechas en diferentes lugares del mundo. Sin embargo, con la incorporación del objeto ```Date``` en Javascript, esta tarea se ha simplificado considerablemente.

Existen también otras formas de comparar fechas en Javascript, como convertir las fechas a formato numérico y luego utilizar operadores lógicos, o usar librerías externas como moment.js. Sin embargo, la forma mostrada anteriormente es la recomendada por su simplicidad y eficiencia.

Es importante tener en cuenta que al comparar fechas en Javascript, se toman en cuenta también los segundos, minutos, horas y otros aspectos del tiempo. Por lo tanto, puede haber casos en los que dos fechas que parecen iguales en su representación, sean consideradas diferentes al realizar la comparación.

## Ver también

- [Documentación de Date en MDN](https://developer.mozilla.org/es/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Moment.js](https://momentjs.com/) - Librería externa para el manejo de fechas en Javascript.