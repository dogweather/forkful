---
title:                "Trabajando con yaml"
html_title:           "TypeScript: Trabajando con yaml"
simple_title:         "Trabajando con yaml"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/working-with-yaml.md"
---

{{< edit_this_page >}}

## Por qué

YAML es un lenguaje de serialización de datos que se ha vuelto muy popular en el mundo de la programación. Es simple y fácil de leer, lo que lo hace una excelente opción para configuraciones y archivos de datos. Además, muchas herramientas y frameworks modernos lo utilizan, por lo que es importante saber cómo trabajar con él.

## Cómo hacerlo

La forma más sencilla de trabajar con YAML en TypeScript es utilizando una librería externa llamada "js-yaml". Esta librería nos permite cargar un archivo YAML y convertirlo en un objeto de JavaScript. Veamos un ejemplo:

```typescript
import * as YAML from 'js-yaml';

const data = YAML.load(`
name: Juan Perez
age: 25
hobbies: 
  - reading
  - hiking
  - cooking
`);

console.log(data.name); // "Juan Perez"
console.log(data.hobbies[1]); // "hiking"
```

Como se puede ver en el ejemplo, podemos cargar un archivo YAML usando el método `load` de la librería y luego acceder a los datos como si fuera un objeto de JavaScript.

Pero ¿qué pasa si queremos convertir un objeto de JavaScript en un archivo YAML? En este caso, podemos utilizar el método `dump` de la librería. Veamos un ejemplo:

```typescript
import * as YAML from 'js-yaml';

const data = {
  name: "Maria Lopez",
  age: 30,
  hobbies: ["painting", "dancing", "traveling"]
};

const yamlData = YAML.dump(data);
console.log(yamlData);
// name: Maria Lopez
// age: 30
// hobbies: 
//   - painting
//   - dancing
//   - traveling
```

En este caso, podemos ver que el objeto de JavaScript se ha convertido en una cadena de texto en formato YAML. Esto es útil cuando queremos guardar datos en archivos de configuración o enviarlos a través de una API.

## Inmersión profunda

Como mencionamos anteriormente, YAML es un formato de serialización de datos simple y fácil de leer, pero también es muy poderoso. A continuación, mostraremos algunas características más avanzadas para trabajar con YAML en TypeScript.

### Comentarios

Los comentarios son una parte importante de cualquier archivo de configuración o archivo de datos. En YAML, los comentarios se pueden agregar utilizando el símbolo `#`, y js-yaml los maneja automáticamente al cargar o convertir un archivo.

### Referencias y anclas

Otra característica interesante de YAML es la posibilidad de utilizar referencias y anclas para reutilizar datos. Esto es especialmente útil cuando tenemos estructuras de datos complejas. Veamos un ejemplo:

```typescript
const data = `
  drinks:
    - &water
      name: Water
      color: clear
      type: still
    - &coffee
      name: Coffee
      color: dark brown
      type: hot
  breakfast:
    - *water
    - *coffee
`;

const breakfastMenu = YAML.load(data);
console.log(breakfastMenu);
// {
//   drinks: [
//    { name: "Water", color: "clear", type: "still" },
//    { name: "Coffee", color: "dark brown", type: "hot"}
//   ],
//   breakfast: [
//    { name: "Water", color: "clear", type: "still" },
//    { name: "Coffee", color: "dark brown", type: "hot"}
//   ]
// }
```

En este ejemplo, hemos creado dos referencias (`&water` y `&coffee`) y luego las hemos utilizado en la sección de "drinks" y "breakfast". De esta forma, no es necesario repetir los mismos datos en diferentes partes del archivo.

### Tipos personalizados

Por último, una característica interesante de YAML es la posibilidad de definir tipos personalizados. Esto nos permite crear nuestras propias estructuras de datos y luego cargarlas o convertirlas a YAML. Veamos un ejemplo:

```typescript
import * as YAML from 'js-yaml';

const data = `
- !user
  name: Maria
  age: 30
- !user
  name: Juan
  age: 25
`;

YAML.addSchema({
  name: 'user',
  kind: 'sequence',
  construct(data) {
    return {
      name: data[0],
      age: data[1]
    };
  }
});
const users = YAML.load(data);
console.log(users); // [{name: "Maria", age: 30}, {name: "Juan", age: 25}]
``