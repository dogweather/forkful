---
title:                "TypeScript: Comenzando un nuevo proyecto"
programming_language: "TypeScript"
category:             "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/starting-a-new-project.md"
---

{{< edit_this_page >}}

# ¿Por qué comenzar un nuevo proyecto en TypeScript?

Empezar un nuevo proyecto siempre es un desafío emocionante, pero ¿por qué deberías considerar usar TypeScript? TypeScript es un lenguaje de programación que se basa en JavaScript, pero agrega características adicionales como tipado estático y soporte para programación orientada a objetos. Estas características pueden ayudar a mejorar la calidad del código y hacer que el proceso de desarrollo sea más eficiente. 

## Cómo hacerlo

Si ya estás familiarizado con JavaScript, aprender TypeScript será fácil ya que comparte una sintaxis similar. Para comenzar a usar TypeScript, simplemente sigue estos pasos:

- Instala TypeScript en tu computadora usando el comando `npm install -g typescript`.
- Crea un archivo con extensión `.ts` y escribe código TypeScript dentro de él.
- Para compilar tu archivo TypeScript a JavaScript, usa el comando `tsc nombreArchivo.ts` en la terminal.
- Luego, ejecuta el archivo JavaScript resultante usando `node nombreArchivo.js` para ver el resultado de tu código.

Aquí hay un ejemplo de código TypeScript que muestra cómo declarar variables con tipos y cómo definir una clase:

```TypeScript
let num1: number = 5;
let num2: number = 10;
let resultado: number = num1 + num2;
console.log(`El resultado es ${resultado}`);

class Persona {
  nombre: string;
  edad: number;

  constructor(nombre: string, edad: number) {
    this.nombre = nombre;
    this.edad = edad;
  }

  saludar() {
    console.log(`Hola, mi nombre es ${this.nombre} y tengo ${this.edad} años.`);
  }
}

const persona1 = new Persona("Juan", 25);
persona1.saludar();
```

El resultado de este código sería:

```
El resultado es 15
Hola, mi nombre es Juan y tengo 25 años.
```

## Profundizando

Ahora que has visto un ejemplo básico de código TypeScript, es importante mencionar que TypeScript también ofrece una gran cantidad de características avanzadas. Por ejemplo, ofrece soporte para el uso de decoradores, módulos y genéricos, entre otros.

Además, TypeScript cuenta con una comunidad activa y una gran cantidad de recursos disponibles en línea para ayudarte en tu aprendizaje. También puedes integrar TypeScript en tus proyectos existentes y gradualmente ir convirtiendo tu código a TypeScript.

En resumen, comenzar un nuevo proyecto en TypeScript puede ser beneficioso para mejorar la calidad y eficiencia de tu código. Y con la gran cantidad de recursos y la comunidad de apoyo, aprender TypeScript puede ser una tarea relativamente sencilla.

## Ver también

- [Documentación oficial de TypeScript](https://www.typescriptlang.org/docs/)
- [Curso de TypeScript en español](https://www.youtube.com/watch?v=DMHz3R6GC6w)
- [Introducción a TypeScript para desarrolladores de JavaScript](https://www.freecodecamp.org/news/learn-typescript-basics-for-javascript-developers/)