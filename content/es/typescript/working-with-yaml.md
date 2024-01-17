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

## ¿Qué y por qué?
Trabajar con YAML es una parte importante del proceso de desarrollo de software para muchos programadores. YAML es un formato de serialización de datos que permite almacenar información en una estructura legible para seres humanos. Gracias a su simplicidad y flexibilidad, se ha convertido en una herramienta popular en el mundo de la programación.

## ¿Cómo hacerlo?
Para trabajar con YAML en TypeScript, primero debemos instalar una librería llamada "yaml", usando el gestor de paquetes npm:

```TypeScript
npm install yaml
```

Luego, podemos importar esta librería en nuestro código TypeScript:

```TypeScript
import * as YAML from 'yaml';
```

A continuación, podemos utilizar la función "dump" para convertir un objeto en un string YAML:

```TypeScript
let data = {
    name: 'Juan',
    age: 25
}

let yamlString = YAML.dump(data);

console.log(yamlString); // Output: name: Juan, age: 25
```

También podemos convertir un string YAML en un objeto de TypeScript usando la función "parse":

```TypeScript
let yamlString = "name: Maria, age: 30";
let data = YAML.parse(yamlString);

console.log(data.name); // Output: Maria
console.log(data.age); // Output: 30
```

## Profundizando
YAML fue creado por Clark Evans en 2001 como una alternativa más legible y estructurada a otros formatos de serialización como XML y JSON. Aunque inicialmente fue desarrollado para trabajos en Python, ha sido adoptado por muchos lenguajes de programación, incluyendo JavaScript y TypeScript.

Además de su simplicidad, una de las ventajas de YAML es que permite comentarios, lo que resulta útil para documentar nuestro código. Sin embargo, algunas de sus desventajas incluyen la falta de soporte por parte de todos los lenguajes de programación y la posibilidad de errores si no se respeta la estructura de indentación.

Existen algunas alternativas a YAML, como JSON y TOML, pero cada una tiene sus propias características y es importante elegir la mejor opción según las necesidades de nuestro proyecto.

## Ver también
- [Página oficial de YAML](https://yaml.org/)
- [Documentación de la librería YAML para TypeScript](https://www.npmjs.com/package/yaml)