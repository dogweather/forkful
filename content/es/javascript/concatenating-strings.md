---
title:    "Javascript: Concatenando cadenas"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/javascript/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por qué

Concatenar strings es una habilidad fundamental en la programación Javascript. Permite combinar y manipular texto para crear cadenas personalizadas y dinámicas para su uso en aplicaciones web y juegos. Sin la capacidad de concatenar strings, muchas tareas en la programación serían mucho más difíciles y complicadas.

## Cómo hacerlo

La concatenación de strings en Javascript puede lograrse utilizando el operador "+" para combinar dos o más cadenas juntas. Por ejemplo:

```Javascript
var nombre = "Juan";
var apellido = "Pérez";
var nombreCompleto = nombre + " " + apellido;

console.log(nombreCompleto); // imprimirá "Juan Pérez"
```

También es posible concatenar una cadena con un valor numérico, ya que el operador "+" también realiza la suma de números. Por ejemplo:

```Javascript
var edad = 25;
var mensaje = "Tengo " + edad + " años";

console.log(mensaje); // imprimirá "Tengo 25 años"
```

Otra forma de concatenar strings es utilizando el método ".concat()", que toma una o más cadenas como argumentos y las combina en una única cadena. Por ejemplo:

```Javascript
var primeraPalabra = "Hola";
var segundaPalabra = "mundo";
var saludo = primeraPalabra.concat(" ", segundaPalabra);

console.log(saludo); // imprimirá "Hola mundo"
```

Es importante tener en cuenta que la concatenación de strings conserva los espacios en blanco y se pueden utilizar múltiples espacios dentro de una cadena. Por ejemplo:

```Javascript
var primerNombre = "Juan";
var segundoNombre = "Pedro";
var nombreCompleto = primerNombre + "      " + segundoNombre;

console.log(nombreCompleto); // imprimirá "Juan      Pedro"
```

## Profundizando

La concatenación de strings en Javascript también se puede hacer utilizando el método ".join()", que combina los elementos de un array en una sola cadena. Por ejemplo:

```Javascript
var pronombres = ["yo", "tú", "él"];
var frase = pronombres.join(" y ");

console.log(frase); // imprimirá "yo y tú y él"
```

También es posible utilizar plantillas de strings para concatenar de manera más eficiente y legible. Estas plantillas contienen placeholders señalados por el símbolo "$" y permiten insertar valores de variables dentro de una cadena. Por ejemplo:

```Javascript
var nombre = "María";
var saludo = `¡Hola ${nombre}! ¿Cómo estás?`;

console.log(saludo); // imprimirá "¡Hola María! ¿Cómo estás?"
```

## Ver también

- [Documentación de MDN sobre la concatenación de strings en Javascript](https://developer.mozilla.org/es/docs/Web/JavaScript/Reference/Global_Objects/String#Concatenar_dos_cadenas)
- [Explicación detallada sobre concatenación de strings en Javascript](https://www.w3schools.com/jsref/jsref_concat_string.asp)
- [Ejemplos prácticos de concatenación de strings en Javascript](https://www.freecodecamp.org/espanol/news/usando-el-operador-de-concatenacion-y-el-metodo-concat-en-javascript/)