---
title:                "Capitalizando una cadena de texto"
html_title:           "Javascript: Capitalizando una cadena de texto"
simple_title:         "Capitalizando una cadena de texto"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?

Capitalizar una cadena significa convertir la primera letra de cada palabra en mayúscula. Los programadores a menudo hacen esto para mejorar la visibilidad y la interpretación de la información.

## Cómo hacerlo:

```Javascript
var str = 'hola mundo'; 
var capitalizado = str.charAt(0).toUpperCase() + str.slice(1); 
console.log(capitalizado);
```

Esto imprimirá en la consola:

```Javascript
"Hola mundo"
```

## Repaso profundo:

Durante muchos años, los desarrolladores han aplicado diferentes métodos para capitalizar cadenas. Esto puede variar desde el uso de una función en bibliotecas como Lodash, hasta implementaciones propias usando Javascript puro.

Algunas alternativas podrían ser:

```Javascript
// Lodash
var _ = require('lodash');
console.log(_.capitalize('hola mundo'));

// Javascript puro
var capMethod2 = 'hola mundo'.split(' ').map(word => word.charAt(0).toUpperCase() + word.slice(1)).join(' ');
console.log(capMethod2);
```

Ambos códigos imprimirán en la consola:

```Javascript
"Hola mundo"
```

Para capitalizar una cadena, Javascript primero accede al primer carácter utilizando `charAt(0)`, luego convierte este carácter a mayúscula con `toUpperCase()`, y finalmente fusiona este carácter con el resto de la cadena omitiendo el primer carácter con `slice(1)`.

## Ver también:

Aquí tienes algunos recursos adicionales sobre cómo capitalizar cadenas:

* Documentación oficial de Javascript: [MDN String.prototype.toUpperCase()](https://developer.mozilla.org/es/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)
* Método Lodash capitalize: [Lodash capitalize](https://lodash.com/docs/4.17.15#capitalize)
* Discusión sobre capitalización en Stackoverflow: [Stackoverflow Capitalize](https://stackoverflow.com/questions/1026069/how-do-i-make-the-first-letter-of-a-string-uppercase-in-javascript)