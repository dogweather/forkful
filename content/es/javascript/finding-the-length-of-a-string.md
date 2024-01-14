---
title:    "Javascript: Encontrando la longitud de una cadena"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Por qué

En programación, es importante poder manipular y trabajar con cadenas de caracteres. En ocasiones, necesitamos saber la cantidad de caracteres que contiene una cadena en particular. En este artículo, aprenderemos cómo encontrar la longitud de una cadena de manera eficiente utilizando Javascript.

## Cómo hacerlo

En Javascript, hay varias formas de encontrar la longitud de una cadena. Aquí te mostramos dos métodos comunes:

### Método 1: Utilizando la propiedad `length`

Usando esta propiedad, podemos obtener la longitud de una cadena de caracteres simplemente accediendo a ella y usando el operador de punto:

```Javascript
let cadena = "¡Hola Mundo!";
let longitud = cadena.length;
console.log(longitud); // Devuelve 12
```

El valor devuelto será un número que representa la cantidad de caracteres que contiene la cadena.

### Método 2: Utilizando el método `slice`

Otra forma de encontrar la longitud de una cadena es utilizar el método `slice`, que nos permite extraer una parte de la cadena especificando el inicio y fin del segmento. Si especificamos como fin la longitud de la cadena, obtendremos la cantidad de caracteres:

```Javascript
let cadena = "¡Hola Mundo!";
let longitud = cadena.slice(0, cadena.length);
console.log(longitud); // Devuelve "¡Hola Mundo!"
```

## Inmersión profunda

Es importante notar que cuando utilizamos la propiedad `length`, no es necesario incluir los paréntesis, ya que se trata de una propiedad, no de un método. Además, la propiedad `length` nos devuelve la cantidad de caracteres en la cadena, incluyendo espacios en blanco y otros caracteres especiales.

Si queremos excluir los espacios en blanco, podemos usar el método `trim` para eliminarlos y luego obtener la longitud de la cadena resultante:

```Javascript
let cadena = "  ¡Hola Mundo!  ";
let longitud = cadena.trim().length;
console.log(longitud); // Devuelve 11
```

También podemos utilizar el método `split` para dividir la cadena en un array y luego obtener la longitud del array resultante:

```Javascript
let cadena = "¡Hola Mundo!";
let array = cadena.split("");
let longitud = array.length;
console.log(longitud); // Devuelve 12
```

## Ver también

- [Propiedad `length` en MDN](https://developer.mozilla.org/es/docs/Web/JavaScript/Referencia/Objetos_globales/String/length)
- [Método `slice` en MDN](https://developer.mozilla.org/es/docs/Web/JavaScript/Referencia/Objetos_globales/String/slice)
- [Método `trim` en MDN](https://developer.mozilla.org/es/docs/Web/JavaScript/Referencia/Objetos_globales/String/trim)
- [Método `split` en MDN](https://developer.mozilla.org/es/docs/Web/JavaScript/Referencia/Objetos_globales/String/split)