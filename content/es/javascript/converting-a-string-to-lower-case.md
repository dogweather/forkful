---
title:                "Convirtiendo una cadena a minúsculas"
html_title:           "Javascript: Convirtiendo una cadena a minúsculas"
simple_title:         "Convirtiendo una cadena a minúsculas"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# ¿Qué y por qué?

Convertir una cadena de texto a minúsculas es una función que permite a los programadores cambiar todas las letras mayúsculas en una cadena de texto a letras minúsculas. Esto puede ser útil para realizar comparaciones de cadenas de texto, ya que las letras mayúsculas y minúsculas son tratadas de manera diferente por la mayoría de los lenguajes de programación.

# Cómo hacerlo:

```Javascript
// Ejemplo 1:
const cadena = "ESTE ES UN EJEMPLO";
console.log(cadena.toLowerCase()); // resultado: "este es un ejemplo"

// Ejemplo 2:
const entrada = prompt("Ingresa una palabra en mayúsculas:");
console.log(entrada.toLowerCase()); // resultado: la entrada del usuario en minúsculas
```

# Profundizando:

- Contexto histórico: La función lowerCase fue introducida en el estándar ES5 de Javascript en 2009.
- Alternativas: Además de la función lowerCase, también existe la función toLowerCase() en Javascript que hace lo mismo.
- Detalles de implementación: La función lowerCase utiliza el conjunto de caracteres Unicode para identificar y cambiar las letras mayúsculas por minúsculas. Esto significa que funciona con cualquier idioma y alfabeto.

# Ver también:

- [Función lowerCase en MDN Web Docs](https://developer.mozilla.org/es/docs/Web/JavaScript/Referencia/Objetos_globales/String/toLowerCase)
- [Función toLowerCase en w3schools](https://www.w3schools.com/jsref/jsref_tolowercase.asp)