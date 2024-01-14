---
title:                "TypeScript: Trabajando con YAML"
simple_title:         "Trabajando con YAML"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/working-with-yaml.md"
---

{{< edit_this_page >}}

## Por qué trabajar con YAML en TypeScript

En el mundo de la programación, existen muchas formas de manejar y organizar datos. Una de ellas es mediante YAML, un formato simple y legible de estructurar información en forma de texto plano. En esta entrada, exploraremos por qué trabajar con YAML puede ser beneficioso y cómo podemos implementarlo en proyectos de TypeScript.

## Cómo hacerlo

Para empezar a trabajar con YAML en TypeScript, lo primero que debemos hacer es instalar la librería `js-yaml` mediante NPM. Una vez instalada, podemos importarla en nuestro código de la siguiente manera:

```TypeScript
import yaml from 'js-yaml';
```

Ahora, podemos utilizar el método `safeLoad()` de la librería para cargar un archivo YAML en nuestro código y convertirlo en un objeto de TypeScript. Por ejemplo, si tenemos un archivo `data.yaml` con la siguiente estructura:

```yaml
titulo: 'Mi gran proyecto'
autor: 'Juan Pérez'
fecha: '2021-08-15'
```

Podemos cargarlo y obtener sus valores de esta manera:

```TypeScript
const data = yaml.safeLoad(fs.readFileSync('data.yaml', 'utf8'));
console.log(data.titulo); // Resultado: Mi gran proyecto
console.log(data.autor); // Resultado: Juan Pérez
console.log(data.fecha); // Resultado: 2021-08-15
```

Incluso podemos trabajar con datos más complejos, como por ejemplo un arreglo de objetos:

```yaml
usuarios:
  - nombre: 'Ana'
    edad: 25
  - nombre: 'Pedro'
    edad: 30
  - nombre: 'María'
    edad: 28
```

Y acceder a ellos de la siguiente forma:

```TypeScript
for (const usuario of data.usuarios) {
  console.log(usuario.nombre); // Resultado: Ana, Pedro, María
  console.log(usuario.edad); // Resultado: 25, 30, 28
}
```

## Profundizando

Además de poder cargar archivos YAML, también podemos crearlos y guardarlos mediante el método `safeDump()` de la librería. Incluso podemos configurar el formato y estilo de salida de nuestros archivos YAML, como por ejemplo incluir comillas o saltos de línea automáticos. Para más información sobre las posibilidades de esta librería, se recomienda consultar su documentación oficial.

Otra ventaja de trabajar con YAML en TypeScript es que nos permite tener un formato más legible y organizado de nuestros datos en lugar de utilizar archivos JSON o CSV. Además, es ampliamente utilizado en el mundo del desarrollo web, lo que nos permite integrar fácilmente nuestros datos YAML en diferentes herramientas y frameworks.

## Ver también

- Documentación oficial de `js-yaml`: https://www.npmjs.com/package/js-yaml
- Ejemplos de uso de YAML en proyectos de TypeScript: https://github.com/thejameskyle/yaml-example
- Introducción a YAML para desarrolladores: https://rollbar.com/blog/yaml-for-programmers/ (en inglés)