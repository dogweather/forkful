---
title:    "Javascript: Comparando dos fechas"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/javascript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## ¿Por qué comparar dos fechas en Javascript?

Hay muchas razones por las que alguien podría querer comparar dos fechas en Javascript. Por ejemplo, puede ser necesario determinar qué fecha es más reciente, o si dos eventos ocurrieron en el mismo día. También es útil al trabajar con datos temporales, como en aplicaciones de calendario o de seguimiento de fechas de vencimiento.

## Cómo hacerlo

Para comparar dos fechas en Javascript, utilizaremos el objeto `Date` y sus métodos `getTime()` y `setTime()`. Primero, creamos dos objetos `Date` con las fechas que queremos comparar:

```Javascript
let fecha1 = new Date(2020, 10, 15); // 15 de noviembre de 2020
let fecha2 = new Date(2021, 2, 20); // 20 de marzo de 2021
```

Luego, utilizamos `getTime()` para obtener el valor numérico de la fecha en milisegundos:

```Javascript
let fecha1Milisegundos = fecha1.getTime(); // 1605398400000
let fecha2Milisegundos = fecha2.getTime(); // 1616203200000
```

Finalmente, podemos comparar los valores para determinar cuál es más reciente:

```Javascript
if (fecha1Milisegundos > fecha2Milisegundos) {
    console.log("La fecha 1 es más reciente que la fecha 2");
} else if (fecha1Milisegundos < fecha2Milisegundos) {
    console.log("La fecha 2 es más reciente que la fecha 1");
} else {
    console.log("Las fechas son iguales");
}

// Output: La fecha 2 es más reciente que la fecha 1
```

## Profundizando

Para obtener más información sobre cómo trabajar con fechas en Javascript, es importante comprender cómo funciona el método `getTime()`. Este método devuelve el tiempo en milisegundos desde el 1 de enero de 1970 a las 00:00:00 UTC. Esta fecha se conoce como "epoch" y es un punto de referencia utilizado por muchos sistemas para medir el tiempo.

Además, es importante tener en cuenta que al crear un objeto `Date`, el mes se indica con un valor numérico entre 0 y 11, significa que en realidad noviembre es el mes 10 y marzo es el mes 2 en nuestros ejemplos anteriores.

## Ver también

- [Documentación de Date en MDN](https://developer.mozilla.org/es/docs/Web/JavaScript/Referencia/Objetos_globales/Date)
- [Cómo trabajar con fechas en Javascript](https://www.w3schools.com/js/js_dates.asp)
- [Métodos para comparar fechas en Javascript](https://flaviocopes.com/how-to-compare-dates-javascript/)