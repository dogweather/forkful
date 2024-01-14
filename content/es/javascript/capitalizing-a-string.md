---
title:    "Javascript: Capitalizar una cadena"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/javascript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

Por qué: La capitalización de cadenas es una técnica común en la programación que ayuda a mejorar la legibilidad y presentación de datos en una aplicación. Es especialmente útil cuando se trabaja con nombres de usuarios, títulos de artículos o cualquier dato que necesite ser mostrado en mayúsculas.

Cómo hacerlo: Para capitalizar una cadena de texto en JavaScript, se puede utilizar el método `toUpperCase()` que transforma todos los caracteres a mayúsculas. También se puede usar el método `charAt()` para acceder al primer carácter de la cadena y usar el método `toUpperCase()` en él, luego unirlo con el resto de la cadena. A continuación se muestra un ejemplo de código y su salida correspondiente:

```Javascript
let cadena = "esto es una prueba"; 
let cadenaCapitalizada = cadena.charAt(0).toUpperCase() + cadena.slice(1); 
console.log(cadenaCapitalizada); // Esto es una prueba
```

En este ejemplo, se usa el método `slice()` para unir el primer carácter capitalizado con el resto de la cadena original.

Profundizando: Es importante tener en cuenta que el método `toUpperCase()` no sólo capitaliza letras, sino que también convierte caracteres especiales y acentos como símbolos y tildes en mayúsculas. Además, este método sólo transforma letras en caracteres ASCII, por lo que no funcionará con caracteres no ASCII como emojis o letras de otros alfabetos.

Si se desea capitalizar una cadena de texto que contenga caracteres no ASCII, se puede utilizar una función personalizada que recorra cada carácter y lo reemplace por su equivalente en mayúsculas. También se pueden utilizar librerías externas como Lodash o String.js que incluyen métodos para capitalizar correctamente cualquier cadena.

Ver también: Para más información sobre el método `toUpperCase()` y otras funciones útiles para manipular cadenas de texto en JavaScript, se pueden consultar los siguientes recursos:

- Documentación oficial de Mozilla: https://developer.mozilla.org/es/docs/Web/JavaScript/Referencia/Objetos_globales/String/toUpperCase
- Guía en español para principiantes en programación: https://uniwebsidad.com/libros/javascript/capitulo-4/metodos-para-manipular-cadenas-de-texto
- Librería Lodash: https://lodash.com/docs/4.17.15#toUpper
- Librería String.js: https://stringjs.com/