---
title:    "Bash: Leyendo argumentos de línea de comandos"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Por qué

Si eres programador o estás aprendiendo a programar, seguramente hayas escuchado hablar de los argumentos de línea de comandos. Pero, ¿por qué son importantes y qué beneficios pueden aportar a tus programas? En esta publicación, exploraremos por qué es crucial aprender a leer los argumentos de la línea de comandos y cómo pueden simplificar tu flujo de trabajo.

## Cómo hacerlo

¿Te has preguntado cómo los programas como Git o Node.js funcionan con diferentes comandos que se escriben en la terminal? La respuesta está en los argumentos de línea de comandos. Estos son valores o cadenas de texto que se pasan junto con el comando al ejecutar un programa. Aquí hay un ejemplo simple de cómo leer argumentos de la línea de comandos en bash:

```bash
#!/bin/bash
echo "Argumentos: $@"
```

El símbolo ```$@``` representa todos los argumentos pasados ​​a la hora de ejecutar el script. Si ejecutamos el script con los argumentos "hola mundo", la salida sería "Argumentos: hola mundo".

También podemos acceder a argumentos específicos utilizando ```$1```, ```$2```, etc. que representan el primer, segundo, etc. argumento pasado.

```bash
#!/bin/bash
echo "Primer argumento: $1"
echo "Segundo argumento: $2"
```

Ahora, si ejecutamos el script con los mismos argumentos "hola mundo", la salida sería "Primer argumento: hola" y "Segundo argumento: mundo". Esto puede ser útil para realizar diferentes acciones según los argumentos proporcionados.

## Profundizando

Además de leer los argumentos simplemente, también podemos procesarlos y validarlos de diferentes maneras. La variable ```$#``` nos da el número total de argumentos pasados, lo que puede ser útil para asegurarse de que se proporcionen todos los argumentos necesarios. También podemos usar ```shift``` para eliminar el primer argumento y, por lo tanto, acceder a los siguientes de manera incremental.

Podemos incluso combinar argumentos en una sola cadena utilizando ```getopts``` y establecer diferentes opciones para manejar diferentes acciones. Esto es útil para crear programas interactivos con múltiples opciones.

## Ver también

- [Bash: Scripting de línea de comandos](https://www.gnu.org/software/bash/manual/html_node/Bash-Scripts.html)
- [Documentación de Node.js sobre procesamiento de argumentos de línea de comandos](https://nodejs.org/docs/latest/api/process.html#process_process_argv)
- [Cómo usar argumentos de línea de comandos en Git](https://git-scm.com/book/tr/v2/Git-Tools-Revision-Selection#_commit_ranges_and_history)

¡Ahora que conoces los fundamentos de los argumentos de línea de comandos en bash, puedes mejorar tus habilidades de programación y hacer que tus programas sean más versátiles y eficientes! ¡Sigue aprendiendo y experimentando con ellos!