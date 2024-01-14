---
title:    "C#: Leyendo argumentos de línea de comando"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

¡Hola amigos! ¿Alguna vez te has preguntado cómo puedes mejorar tu programación en C#? Una forma de hacerlo es dominar la técnica de leer argumentos de línea de comandos. En este blog post, te explicaré por qué es importante y cómo puedes hacerlo en tu propio código. ¡Vamos a sumergirnos en el mundo de los argumentos de línea de comandos!

## Por qué
¿Por qué deberías molestarte en leer argumentos de línea de comandos en tu código? Bueno, en primer lugar, puede ahorrarte mucho tiempo y esfuerzo. Imagina que tienes una aplicación de línea de comandos que acepta ciertos argumentos para realizar diferentes tareas. Si tu programa es capaz de leer esos argumentos de forma automática, puedes evitar tener que solicitar al usuario que ingrese los valores cada vez que ejecute el programa.

Además, leer argumentos de línea de comandos también puede mejorar la funcionalidad de tu programa. Puedes hacer que sea más versátil y fácil de usar para los usuarios, ya que pueden simplemente ingresar los valores deseados al ejecutar el programa en lugar de tener que navegar por una interfaz gráfica compleja.

## Cómo hacerlo
Ahora que sabes por qué es importante leer argumentos de línea de comandos, es hora de aprender cómo hacerlo. En C#, puedes hacerlo utilizando la clase `Environment` y su propiedad `GetCommandLineArgs()`. Esta propiedad devuelve un array con todos los argumentos ingresados al ejecutar el programa en la línea de comandos.

Por ejemplo, si quisieras leer el primer argumento ingresado, podrías hacerlo de la siguiente manera:

```C#
string primerArgumento = Environment.GetCommandLineArgs()[0];
```

Y si quisieras leer un segundo o tercer argumento, simplemente tendrías que cambiar el índice en el array. Una vez que hayas obtenido los argumentos, puedes utilizarlos en tu código como lo harías con cualquier otra variable.

## Profundizando
¿Quieres saber más sobre cómo funcionan los argumentos de línea de comandos? Aquí hay algunos detalles adicionales que pueden ser útiles para ti.

- Puedes utilizar la clase `Process` para ejecutar tu programa con argumentos desde otra aplicación o script.
- Puedes incluir opciones en tus argumentos utilizando comillas dobles. Por ejemplo, `"Nombre de usuario"` en lugar de solo `Nombre de usuario`.
- Puedes utilizar la clase `CommandLineParser` para facilitar la lectura y validación de los argumentos.

Espero que este artículo te haya ayudado a comprender la importancia de leer argumentos de línea de comandos en tu código en C#. Ahora es tu turno de practicar y experimentar con esta técnica en tus propios proyectos. ¡Revisa los enlaces a continuación para obtener más información!

## Ver también
- [Documentación de Environment.GetCommandLineArgs](https://docs.microsoft.com/es-es/dotnet/api/system.environment.getcommandlineargs?view=net-5.0)
- [Uso de argumentos de línea de comandos en aplicaciones .NET Core](https://docs.microsoft.com/es-es/dotnet/core/tools/dotnet-run?tabs=netcore2x#arguments)
- [How to Read Command Line Arguments in C#](https://khan4019.github.io/front-end-Interview-Questions/dotnet.html#?question=how-to-read-command-line-arguments-in-c)
- [Usando CommandLineParser en aplicaciones de línea de comandos](https://www.codeproject.com/Articles/3111/Using-Command-Line-Arguments-in-Your-Visual-Studio)