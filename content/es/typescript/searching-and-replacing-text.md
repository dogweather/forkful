---
title:    "TypeScript: Buscando y reemplazando texto"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Por qué
Reemplazar texto es una tarea común en programación. Ya sea que esté cambiando nombres de variables o corrigiendo errores de ortografía, la capacidad de buscar y reemplazar texto de manera eficiente es esencial para un flujo de trabajo eficaz en TypeScript.

## Cómo hacerlo
La sintaxis básica para buscar y reemplazar texto en TypeScript es la siguiente:

```TypeScript
texto.replace(valorBuscado, valorReemplazado);
```

Este método toma dos parámetros: el valor que desea buscar y el valor con el que desea reemplazarlo. Por ejemplo, si desea reemplazar la palabra "gato" con "perro" en el siguiente texto:

```TypeScript
let texto = "Me encanta mi gato doméstico";

texto = texto.replace("gato", "perro");

console.log(texto);

// Output: Me encanta mi perro doméstico
``` 

También puede usar expresiones regulares para realizar búsquedas más complejas y reemplazos utilizando las siguientes sintaxis:

```TypeScript
texto.replace(/expresiónRegular/g, valorReemplazado);
```

Por ejemplo, si desea reemplazar todas las apariciones de la palabra "es" con "son" en el siguiente texto:

```TypeScript
let texto = "Ellos son muy simpáticos";

texto = texto.replace(/es/g, "son");

console.log(texto);

// Output: Ellos son muy simpáticos
```  

## Profundizando
Finalmente, es importante destacar algunas cosas a tener en cuenta al buscar y reemplazar texto en TypeScript:

- El método `replace` no modifica el valor original de la variable. Debe asignar el resultado a una nueva variable o reemplazarla en la misma variable, como se mostró en los ejemplos anteriores.

- Si desea reemplazar todas las apariciones de un valor, debe usar una expresión regular con la bandera `g` para que se realice una búsqueda global. De lo contrario, solo se reemplazará la primera aparición del valor.

- El método `replace` es sensible a mayúsculas y minúsculas. Si desea realizar un reemplazo insensible a mayúsculas y minúsculas, puede usar la bandera `i` en la expresión regular.

## Ver también
Aquí hay algunos recursos adicionales para aprender más sobre cómo buscar y reemplazar texto en TypeScript:

- [Documentación oficial de TypeScript sobre el método `replace`](https://www.typescriptlang.org/docs/handbook/enums.html)
- [Tutorial de W3Schools sobre expresiones regulares en JavaScript](https://www.w3schools.com/js/js_regexp.asp)