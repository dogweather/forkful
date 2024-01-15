---
title:                "Escribiendo pruebas"
html_title:           "Javascript: Escribiendo pruebas"
simple_title:         "Escribiendo pruebas"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/writing-tests.md"
---

{{< edit_this_page >}}

# Por qué escribir pruebas en Javascript

Escribir pruebas en Javascript puede parecer una tarea tediosa e innecesaria para algunos desarrolladores. Sin embargo, hay varias razones importantes por las que vale la pena dedicar tiempo a escribir pruebas para tu código.

Primero, las pruebas aseguran que tu código funcione correctamente en diferentes situaciones y escenarios. Esto es crucial para garantizar la calidad del código y prevenir errores en el futuro. Además, escribir pruebas también facilita la detección y resolución de errores, lo que ahorra tiempo y esfuerzo en el proceso de desarrollo.

# Cómo escribir pruebas en Javascript

Escribir pruebas en Javascript puede realizarse de diferentes maneras, pero una de las herramientas más populares para ello es Jest. Jest es una biblioteca de pruebas creada por Facebook que facilita la escritura y ejecución de pruebas en Javascript.

Para comenzar a escribir pruebas en Jest, primero debes instalarlo en tu proyecto utilizando npm o yarn. Una vez instalado, puedes crear un archivo de prueba con la extensión ".test.js" y escribir tus pruebas utilizando las funciones y métodos proporcionados por Jest.

Por ejemplo, si tenemos una función "sumar", podemos escribir una prueba utilizando una de las funciones de Jest: "expect". El código se vería así:

```Javascript
function sumar(a, b) {
  return a + b;
}

test('suma 2 + 3 y devuelve 5', () => {
  expect(sumar(2, 3)).toBe(5);
});
```

En este ejemplo, estamos probando si nuestra función "sumar" devuelve el resultado esperado cuando se le pasan dos números.

Una vez que hayas escrito tus pruebas, puedes ejecutarlas con el comando "npm test" en tu terminal. Jest te mostrará los resultados de tus pruebas y te indicará si alguna de ellas falla.

# Deep Dive en la escritura de pruebas en Javascript

Escribir pruebas en Javascript no solo se trata de utilizar una biblioteca de pruebas como Jest, sino también de seguir buenas prácticas y patrones para asegurar que tus pruebas sean efectivas y fáciles de mantener.

Algunas de estas prácticas incluyen escribir pruebas para todos los escenarios posibles, estructurar tus pruebas de manera lógica y clara, y tener una buena cobertura de código con tus pruebas.

También es importante tener en cuenta que las pruebas deben ser independientes y no depender de otras pruebas. Esto es para asegurar que si una prueba falla, no afectará a las demás y podrás identificar fácilmente el problema.

En resumen, escribir pruebas en Javascript no solo garantizará que tu código funcione correctamente, sino que también te ayudará a construir un código más sólido y fácil de mantener a largo plazo.

# Ver también
- [Documentación de Jest](https://jestjs.io/)
- [Artículo sobre buenas prácticas para escribir pruebas en Javascript](https://betterprogramming.pub/how-to-write-great-javascript-unit-tests-9efe4464e0c5)
- [Curso gratuito de pruebas en Javascript en Platzi](https://platzi.com/cursos/testing-javascript/)