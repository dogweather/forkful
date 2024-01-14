---
title:    "Bash: Leyendo argumentos de línea de comandos"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/bash/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por qué

Antes de sumergirnos en cómo leer argumentos de línea de comando en Bash, es importante entender por qué es una habilidad importante para cualquier programador. Al utilizar argumentos de línea de comando, podemos aumentar la eficiencia y flexibilidad de nuestros programas al permitir que los usuarios personalicen su funcionamiento a través de la entrada de comandos específicos. Además, el uso de argumentos de línea de comando también puede facilitar la automatización de tareas y procesos en nuestro flujo de trabajo.

## Cómo hacerlo

Para leer argumentos de línea de comando en Bash, podemos utilizar el built-in variable `$@` en nuestro código. Esta variable contiene una lista de todos los argumentos ingresados por el usuario al ejecutar el programa. A continuación, podemos utilizar la estructura de control `for` para iterar a través de los argumentos y realizar acciones basadas en ellos. Veamos un ejemplo:

```Bash
for arg in "$@"
do
    case $arg in
        -h|--help)
            echo "Este es un mensaje de ayuda."
            ;;
        -v|--version)
            echo "La versión del programa es 1.0."
            ;;
        *)
            echo "Argumento desconocido: $arg. Utilice -h o --help para ver las opciones disponibles."
            exit 1
            ;;
    esac
done
```

En este ejemplo, estamos utilizando la estructura de control `case` para realizar diferentes acciones según el argumento ingresado por el usuario. Si se proporciona el argumento `-h` o `--help`, se imprimirá un mensaje de ayuda. Si se ingresa `-v` o `--version`, se mostrará la versión del programa. Y si se proporciona cualquier otro argumento, se imprimirá un mensaje de error y el programa se terminará.

## Profundizando

Además de la estructura de control `for`, también podemos utilizar la variable `$#` para obtener el número total de argumentos ingresados. Además, podemos acceder a cada argumento individualmente utilizando `$1`, `$2`, `$3`, etc. Esto puede ser útil cuando necesitamos trabajar con argumentos específicos en nuestro código. También podemos utilizar la estructura de control `if` para realizar acciones basadas en la presencia o ausencia de ciertos argumentos. Por ejemplo:

```Bash
if [ $# -lt 2 ]; then
    echo "Se requieren al menos dos argumentos para ejecutar este programa."
    exit 1
fi
```

En este caso, estamos utilizando un condicional para verificar si se han ingresado al menos dos argumentos. Si no es así, se imprimirá un mensaje de error y el programa se terminará.

## Ver también

- [Documentación oficial de Bash](https://www.gnu.org/software/bash/manual/bash.html)
- [Tutorial de argumentos de línea de comando en Bash](https://linuxize.com/post/how-to-use-getopts-to-parse-a-script-options/)