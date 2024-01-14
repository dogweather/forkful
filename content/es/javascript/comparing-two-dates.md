---
title:                "Javascript: Comparando dos fechas"
programming_language: "Javascript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/comparing-two-dates.md"
---

{{< edit_this_page >}}

##¿Por qué comparar dos fechas en la programación?

A menudo, en la programación, necesitamos comparar dos fechas para realizar diferentes acciones en nuestras aplicaciones. Ya sea para verificar si una fecha es anterior, posterior o igual a otra, o para realizar cálculos basados en fechas, conocer cómo comparar fechas en Javascript es una habilidad importante para cualquier programador.

##¿Cómo hacerlo en Javascript?

Para comparar dos fechas en Javascript, primero debemos crear dos objetos de fecha usando la función `new Date()`. Luego, podemos utilizar los operadores de comparación (`>`, `<`, `>=`, `<=`) para comparar las fechas entre sí.

Veamos un ejemplo de cómo comparar dos fechas en Javascript:

```Javascript
let fecha1 = new Date(2021, 4, 21); // 21 de mayo de 2021
let fecha2 = new Date(2020, 8, 10); // 10 de septiembre de 2020

if (fecha1 > fecha2) {
  console.log("Fecha 1 es posterior a Fecha 2");
} else if (fecha1 < fecha2) {
  console.log("Fecha 1 es anterior a Fecha 2");
} else {
  console.log("Fecha 1 es igual a Fecha 2");
}
```

En este ejemplo, creamos dos objetos de fecha con diferentes valores y luego utilizamos el operador de comparación `>` para imprimir en la consola si la Fecha 1 es posterior, anterior o igual a la Fecha 2.

##Profundizando en la comparación de fechas

Aunque comparar dos fechas en Javascript puede parecer simple, hay más detalles que debemos tener en cuenta. Por ejemplo, al comparar fechas, también se tienen en cuenta la hora y los segundos. Por lo tanto, dos fechas con la misma fecha pero con diferentes horas o segundos serán consideradas diferentes al compararlas.

Otro aspecto importante es que, al igual que otros objetos en Javascript, los objetos de fecha también tienen métodos que nos permiten realizar diferentes cálculos con las fechas, como `getFullYear()`, `getMonth()`, `getDate()`, entre otros.

Si quieres conocer más sobre cómo trabajar con fechas en Javascript, te recomiendo revisar la documentación oficial de MDN sobre el objeto Date.

##Ver también:

- [MDN web docs: Trabajando con fechas en Javascript](https://developer.mozilla.org/es/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [W3Schools: Objeto Date en Javascript](https://www.w3schools.com/js/js_dates.asp)
- [Tutorial de comparación de fechas en Javascript](https://www.delftstack.com/es/howto/javascript/compare-date-objects-in-javascript/)