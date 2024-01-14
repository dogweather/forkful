---
title:                "TypeScript: Iniciando un nuevo proyecto"
simple_title:         "Iniciando un nuevo proyecto"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Por qué

Empezar un nuevo proyecto puede ser una idea emocionante y abrumadora al mismo tiempo. Sin embargo, hay muchas razones por las que puede ser beneficioso para ti empezar un proyecto en TypeScript. Esta popular opción de lenguaje de programación ofrece ventajas como tipado estático, mayor escalabilidad, y la posibilidad de utilizar nuevas características como clases e interfaces.

## Cómo hacerlo

Para empezar un proyecto en TypeScript, primero necesitas asegurarte de tener instalado Node.js y su gestor de paquetes npm. A continuación, puedes crear una carpeta para tu proyecto y abrir una terminal en ella. Desde la terminal, puedes iniciar un proyecto de Node.js utilizando `npm init`, que te guiará a través del proceso de configuración. Una vez que hayas completado el proceso, puedes instalar TypeScript y sus dependencias utilizando el comando `npm install --save-dev typescript @types/node`.

Ahora que tienes todo lo necesario para empezar a programar en TypeScript, puedes crear un archivo `app.ts` y empezar a escribir tu código. Para compilar tu código en JavaScript, utiliza el comando `tsc app.ts`. Esto generará un archivo `app.js`, que puedes ejecutar con el comando `node app.js`. ¡Felicidades! ¡Has empezado tu proyecto en TypeScript!

Aquí hay un ejemplo de un programa simple que imprime "¡Hola, mundo!" por consola:

```TypeScript
function saludar(nombre: string) {
  console.log("¡Hola, " + nombre + "!");
}

saludar("mundo");
```

La salida de este programa sería `¡Hola, mundo!` en la consola.

## Profundizando

Si quieres profundizar en TypeScript y aprender aún más sobre cómo empezar un proyecto, hay muchos recursos disponibles. Puedes consultar la documentación oficial de TypeScript, que ofrece una guía completa y detallada para empezar a programar en este lenguaje. También puedes unirte a comunidades en línea, como foros y grupos de Slack, para hacer preguntas y aprender de otros desarrolladores experimentados.

Otra forma de profundizar en TypeScript es creando proyectos personales y experimentando con diferentes características y funcionalidades. La práctica siempre es la mejor forma de mejorar tus habilidades y conocimientos.

## Ver también

- [Documentación oficial de TypeScript](https://www.typescriptlang.org/docs/)
- [Página de inicio de Node.js](https://nodejs.org/es/)
- [Grupo de Slack de TypeScript en Español](https://typescriptes.github.io/)