---
title:    "Gleam: Comenzar un nuevo proyecto."
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/gleam/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Por qué empezar un nuevo proyecto en Gleam

Si estás buscando un lenguaje de programación moderno y eficiente para trabajar en tus proyectos, Gleam es la opción perfecta. Con su tipado estático, alto rendimiento y sintaxis amigable, Gleam te permite desarrollar aplicaciones robustas y escalables. En este artículo, aprenderás cómo dar tus primeros pasos en este emocionante lenguaje.

## Cómo comenzar un nuevo proyecto en Gleam

Para empezar un nuevo proyecto en Gleam, primero debes asegurarte de tener instalado el compilador y el gestor de paquetes de Gleam en tu computadora. A continuación, sigue estos pasos:

1. Crea una nueva carpeta para tu proyecto y ábrela en tu editor de código favorito. Luego, inicia el gestor de paquetes con `gleam.exe new`.

2. Se te guiará a través de unas pocas preguntas para configurar tu proyecto. Esto incluye elegir un nombre para tu proyecto, una descripción, una licencia y si quieres incluir soporte para herramientas de calidad de código como formateadores y analizadores estáticos.

3. Una vez que hayas respondido todas las preguntas, el gestor de paquetes creará una estructura básica para tu proyecto, incluyendo un archivo `gleam.toml` con todas las configuraciones necesarias.

4. Ahora puedes empezar a escribir tu código en el archivo `src/main.gleam` que se ha creado para ti. Puedes utilizar cualquiera de las librerías disponibles en el repositorio de paquetes de Gleam o incluso crear tus propias librerías.

5. Una vez que hayas terminado de escribir tu código, puedes compilarlo con `gleam.exe build` y ejecutarlo con `gleam.exe run`.

¡Felicidades! Has creado tu primer proyecto en Gleam.

## Profundizando en la creación de proyectos en Gleam

En Gleam, un proyecto se estructura en módulos, que son como bloques de construcción para tus aplicaciones. Puedes crear tantos módulos como necesites en tu proyecto y luego utilizarlos en otros módulos o incluso en otros proyectos.

Al crear un proyecto, se te proporciona un archivo `gleam.toml` que contiene información sobre el proyecto y sus dependencias. Puedes modificar este archivo para agregar nuevas dependencias o cambiar la configuración de tu proyecto.

También puedes utilizar la herramienta de formateo de Gleam para asegurarte de que tu código esté bien escrito y fácil de leer. Solo necesitas ejecutar `gleam.exe format` en la terminal y el formateador hará el resto por ti.

¡Ahora estás listo para dar rienda suelta a tu creatividad y comenzar a desarrollar aplicaciones increíbles en Gleam!

## Ver también

- [Documentación de Gleam](https://gleam.run/getting-started/)
- [Repositorio de paquetes de Gleam](https://gleam.run/libraries/)
- [Proyectos de ejemplo en Gleam](https://github.com/gleam-lang/gleam/tree/master/examples)