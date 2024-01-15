---
title:                "Trabajando con json"
html_title:           "TypeScript: Trabajando con json"
simple_title:         "Trabajando con json"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/working-with-json.md"
---

{{< edit_this_page >}}

## Por qué

Trabajar con JSON es una habilidad esencial para cualquier programador TypeScript, ya que es un formato de intercambio de datos ampliamente utilizado en aplicaciones web y móviles. Al aprender a trabajar con JSON, podrás manejar de manera eficiente y efectiva la información en tus proyectos.

## Cómo hacerlo

Para trabajar con JSON en TypeScript, primero debes comprender su estructura básica. JSON es un formato de datos basado en texto que se utiliza para almacenar y transmitir datos. Consiste en una serie de pares de "clave: valor" separados por comas y encerrados entre llaves. Por ejemplo:

```TypeScript
const persona = { 
  nombre: "María", 
  edad: 25, 
  ciudad: "Madrid"
};

console.log(persona.nombre); // Output: "María"
```

En el ejemplo anterior, hemos definido un objeto llamado "persona" con tres propiedades: nombre, edad y ciudad. Luego, utilizamos la sintaxis de punto para acceder a la propiedad "nombre" de ese objeto y mostrar su valor en la consola.

También es posible trabajar con JSON en TypeScript utilizando la función `JSON.stringify()` para convertir un objeto en formato JSON y `JSON.parse()` para convertir una cadena JSON en un objeto. Por ejemplo:

```TypeScript
const persona = { 
  nombre: "María", 
  edad: 25, 
  ciudad: "Madrid"
};

const personaJSON = JSON.stringify(persona);
console.log(personaJSON); // Output: {"nombre":"María","edad":25,"ciudad":"Madrid"}

const personaObjeto = JSON.parse(personaJSON);
console.log(personaObjeto.nombre); // Output: "María"
```

## Profundizando

Además de los objetos, también puedes trabajar con arreglos JSON en TypeScript. Un arreglo JSON es una lista ordenada de valores encerrados entre corchetes y separados por comas. Por ejemplo:

```TypeScript
const frutas = ["manzana", "plátano", "fresa"];
console.log(frutas[0]); // Output: "manzana"
```

También puedes utilizar la función `JSON.stringify()` y `JSON.parse()` para trabajar con arreglos JSON de manera similar a los objetos.

Otra funcionalidad útil al trabajar con JSON en TypeScript es la validación de datos. Puedes utilizar la librería `ajv` para validar que un objeto o cadena JSON cumpla con un esquema específico.

## Ver también

- [Documentación oficial de TypeScript sobre JSON](https://www.typescriptlang.org/docs/handbook/declaration-files/do-s-and-don-ts.html) 
- [JSON en TypeScript - Tutorial para principiantes](https://blog.bitsrc.io/json-in-typescript-tutorial-for-beginners-b17f4af72508) 
- [Librería ajv para validación de datos en TypeScript](https://github.com/epoberezkin/ajv)