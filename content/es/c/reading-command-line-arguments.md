---
title:                "Leyendo argumentos de línea de comandos"
html_title:           "C: Leyendo argumentos de línea de comandos"
simple_title:         "Leyendo argumentos de línea de comandos"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Leer argumentos de línea de comando es una práctica común entre los programadores de C. Consiste en obtener información específica (como valores o opciones) desde el momento en que se invoca un programa mediante la línea de comandos. Esta técnica es utilizada para hacer más flexible y personalizable la ejecución de un programa.

## ¿Cómo hacerlo?

Para leer argumentos de línea de comando en C, se utiliza la función "main" con dos parámetros: "argc" y "argv". "argc" representa el número de argumentos ingresados y "argv" es un arreglo de cadenas que contiene cada uno de ellos. Veamos un ejemplo:

```
int main(int argc, char *argv[]) {
    printf("Se han ingresado %d argumentos\n", argc);
    printf("El argumento en la posición 1 es: %s\n", argv[1]);
    return 0;
}
```

Si ejecutamos el programa con la siguiente línea de comando:

```
./my_program arg1 arg2
```

Obtendremos la siguiente salida:

```
Se han ingresado 3 argumentos
El argumento en la posición 1 es: arg1
```

Podemos ver que se han ingresado 3 argumentos, incluyendo el nombre del programa ("my_program") y que el argumento en la posición 1 es "arg1".

## Sumergirse en los detalles

Los argumentos de línea de comando existen desde los inicios de los sistemas operativos de línea de comandos, como Unix y DOS. Con el auge de las interfaces gráficas, hoy en día son menos utilizados, pero siguen siendo una herramienta útil para los programadores.

Una alternativa al uso de argumentos de línea de comando es utilizar variables de entorno, que son un conjunto de variables globales accesibles para todos los procesos en un sistema operativo. Sin embargo, leer las variables de entorno puede ser más complejo que leer los argumentos de línea de comando, ya que se deben utilizar funciones específicas para ello.

En cuanto a la implementación, las funciones "main" y "argc/argv" son estándar en C y son soportadas por la mayoría de los compiladores y sistemas operativos. Sin embargo, algunas plataformas pueden tener sus propias extensiones o limitaciones en cuanto al número de argumentos o su longitud máxima.

## Ver también

- [Documentación oficial de la función "main" en C](https://www.gnu.org/software/libc/manual/html_node/Program-Arguments.html#Program-Arguments)
- [Tutorial sobre argumentos de línea de comando en C](http://www.learn-c.org/en/Command_Line_Arguments)