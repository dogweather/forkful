---
title:    "Javascript: Búsqueda y reemplazo de texto"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/javascript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por qué

El método de buscar y reemplazar texto es una herramienta esencial en la programación JavaScript. Esta función te permite encontrar una cadena de texto específica y reemplazarla con otra, lo que puede ser útil para corregir errores o realizar cambios en una gran cantidad de código.

## Cómo hacerlo

Para utilizar la función de buscar y reemplazar en JavaScript, puedes seguir estos pasos:

1. Primero, debes identificar la cadena de texto que deseas buscar y reemplazar.
2. Utiliza el método `replace()` en esa cadena de texto.
3. Dentro de los paréntesis, escribe la cadena de texto que deseas reemplazar y después la nueva cadena de texto que deseas utilizar.
4. Si la cadena de texto se encuentra en más de una ubicación, puedes utilizar la bandera `g` para realizar el reemplazo en todas las ocurrencias.

A continuación, se presenta un ejemplo de cómo utilizar este método en código JavaScript:

```Javascript
let texto = "Hola amigos, ¿cómo están?";
let nuevoTexto = texto.replace("hola", "buen día");

console.log(nuevoTexto);
```

Output:
```
Buen día amigos, ¿cómo están?
```

## Profundizando

Además de reemplazar una cadena de texto específica, el método `replace()` también te permite utilizar expresiones regulares para buscar patrones en un texto y reemplazarlos. Esto es especialmente útil cuando deseas realizar cambios en una gran cantidad de texto.

Por ejemplo, si deseas reemplazar todas las letras mayúsculas de una palabra por minúsculas, puedes utilizar una expresión regular en vez de escribir cada letra individualmente. Una expresión regular se escribe entre dos barras diagonales (`/.../`) y puede contener diferentes símbolos para buscar patrones.

A continuación, se muestra un ejemplo de cómo utilizar una expresión regular en el método `replace()`:

```Javascript
let texto = "hOlA mUnDo";
let nuevoTexto = texto.replace(/[A-Z]/g, (letra) => letra.toLowerCase());
// El primer parámetro es la expresión regular y el segundo es una función que convierte la letra a minúscula.

console.log(nuevoTexto);
```

Output:
```
hola mundo
```

## Ver también

- [Método `replace()` en Mozilla Developer Network](https://developer.mozilla.org/es/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [Expresiones regulares en JavaScript en W3Schools](https://www.w3schools.com/js/js_regexp.asp)