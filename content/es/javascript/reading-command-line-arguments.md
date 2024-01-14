---
title:    "Javascript: Leyendo argumentos de la línea de comandos"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

#¿Por qué leer argumentos de línea de comandos?

Si estás aprendiendo Javascript o ya tienes experiencia en programación, probablemente sepas que existen diferentes maneras de recibir datos en un programa, como inputs del usuario o archivos externos. Pero ¿sabías que también se pueden pasar argumentos directamente en la línea de comandos al ejecutar un programa? Esta es una herramienta útil para automatizar tareas o personalizar la ejecución de un programa.

##Cómo hacerlo

Primero, asegúrate de tener instalado Node.js en tu computadora. Luego, sigue estos pasos:

1. Crea un archivo de Javascript con cualquier nombre (por ejemplo, "arguments.js").
2. Abre este archivo en tu editor de código favorito y comienza escribiendo el esqueleto de un programa básico de Javascript:
```
JavaScript
console.log("¡Hola, mundo!");
```
3. Ahora, queremos pasar un argumento en la línea de comandos al ejecutar este programa. Para eso, abrimos nuestra terminal y escribimos "node" seguido del nombre del archivo y el argumento que queremos pasar, separados por un espacio:
```
Bash
node arguments.js argumento
```
En este ejemplo, estamos pasando el argumento "argumento" al programa.

4. Dentro del archivo de Javascript, podemos acceder a este argumento utilizando el objeto process.argv. Este objeto contiene un array con todos los argumentos pasados en la línea de comandos. En nuestro ejemplo, sería así:
```
JavaScript
let argumento = process.argv[2];
console.log(argumento); // output: argumento
```
5. Ahora puedes utilizar este argumento en tu programa como mejor te parezca. Por ejemplo, puedes utilizarlo para realizar una acción específica o como input para una función.

##Profundizando

El objeto process.argv en realidad contiene tres elementos: el path de Node, el path del archivo y los argumentos pasados. Por lo tanto, si deseas acceder solo a los argumentos pasados, recuerda que están a partir de la posición 2 en el array.

También puedes pasar varios argumentos separados por espacios en la línea de comandos y acceder a ellos de manera similar. Por ejemplo:
```
Bash
node arguments.js argumento1 argumento2 argumento3
```
Y en tu archivo de Javascript tendrás un array con los tres argumentos como elementos.

## Véase también

- [Documentación oficial de Node.js sobre process.argv](https://nodejs.org/docs/latest-v12.x/api/process.html#process_process_argv)
- [Tutorial de FreeCodeCamp sobre argumentos de línea de comandos en Node.js](https://www.freecodecamp.org/news/how-to-read-command-line-arguments-in-nodejs/)
- [Video tutorial de automatización con argumentos de línea de comandos en Node.js](https://www.youtube.com/watch?v=fgTGADljAeg)