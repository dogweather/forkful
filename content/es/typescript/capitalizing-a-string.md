---
title:    "TypeScript: *Capitalizando una cadena"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Por qué

Existen varias razones por las cuales alguien podría querer capitalizar una cadena de texto en TypeScript. Podría ser para mejorar la legibilidad del código, para cumplir con ciertos estándares de programación o simplemente para tener una salida de datos consistente.

## Cómo hacerlo

La forma más sencilla de capitalizar una cadena de texto en TypeScript es utilizando el método `toUpperCase()` de la clase `String`. Este método convierte todos los caracteres de una cadena en mayúsculas y devuelve una nueva cadena con la transformación.

```TypeScript
const cadena = "hola mundo";
const cadenaCapitalizada = cadena.toUpperCase();

console.log(cadenaCapitalizada); // Salida: HOLA MUNDO
```

Otra opción es utilizar la propiedad `charAt()` junto con el método `toUpperCase()` para capitalizar únicamente la primera letra de la cadena.

```TypeScript
const cadena = "hola mundo";
const primeraLetra = cadena.charAt(0).toUpperCase();
const restoCadena = cadena.slice(1);

console.log(primeraLetra + restoCadena); // Salida: Hola mundo
```

También es posible utilizar expresiones regulares para capitalizar cada palabra de una cadena. En el siguiente ejemplo, se utiliza el método `replace()` junto con la expresión regular `/\b\w/g` para reemplazar cada letra del primer caracter de una palabra por su versión en mayúsculas.

```TypeScript
const cadena = "bienvenidos a mi blog";
const cadenaCapitalizada = cadena.replace(/\b\w/g, (letra) => letra.toUpperCase());

console.log(cadenaCapitalizada); // Salida: Bienvenidos A Mi Blog
```

## Profundizando

Existen varias formas de capitalizar una cadena de texto en TypeScript, cada una con sus propias particularidades y ventajas. Se recomienda investigar más sobre el tema y elegir la opción que mejor se adapte a las necesidades de cada proyecto.

## Ver también

- [Documentación de `toUpperCase()` en MDN](https://developer.mozilla.org/es/docs/Web/JavaScript/Referencia/Objetos_globales/String/toUpperCase)
- [Documentación de `charAt()` en MDN](https://developer.mozilla.org/es/docs/Web/JavaScript/Referencia/Objetos_globales/String/charAt)
- [Documentación de `replace()` en MDN](https://developer.mozilla.org/es/docs/Web/JavaScript/Referencia/Objetos_globales/String/replace)
- [Expresiones regulares en TypeScript](https://www.typescriptlang.org/docs/handbook/regular-expressions.html)